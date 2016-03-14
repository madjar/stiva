module Epop (Epop, runEpop, getAvailableTasks, getTasks, setTasks, isLoggedIn) where

import           ClassyPrelude                hiding (Element)
import           Control.Concurrent           (threadDelay)
import           Control.Error
import           Control.Monad.Extra          (findM)
import           Data.List.Extra              (dropEnd, elemIndex, groupSort,
                                               (!!))
import           Data.Time                    (addDays)
import           Test.WebDriver
import           Test.WebDriver.Commands.Wait (expect, expectAlertOpen,
                                               unexpected, waitUntil)
import           Test.WebDriver.Utils         (urlEncode)

import           Types

type Credentials = (String, String)
type Epop = ExceptT String (ReaderT Credentials WD)

-- TODO handle the "Planned" checkbox
openEpop :: String -> Epop ()
openEpop p = do (user, pass) <- ask
                openPage ("https://" ++ enc user ++ ":" ++ enc pass ++ "@" ++ epopRoot ++ p)
  where epopRoot = "axa3661.msepmonline.net/ProjectServer3661/"
        enc = unpack . urlEncode . pack

runEpop :: String -> String -> Epop a -> ExceptT String IO a
runEpop user pass = handleAny handler . mapExceptT (runSession conf . finallyClose . flip runReaderT creds)
  where conf = defaultConfig { wdHost = "192.168.99.100" }
        creds = (user, pass)
        handler e = throwE (show e)

listWeekLines :: Epop [Element]
listWeekLines =
  do openEpop  "_layouts/15/pwa/Timesheet/MyTSSummary.aspx"
     table <- findElem (ById "ctl00_ctl00_PlaceHolderMain_PWA_PlaceHolderMain_idGrid")
     findElemsFrom table (ByXPath "tbody/tr[*]/td[2]/a")

listTimeSheets :: Epop [Week]
listTimeSheets =
  do weekLines <- listWeekLines
     traverse toWeek weekLines
  where toWeek el =
          do Just weekText <- attr el "textContent"
             parseWeek weekText


getStatusText :: Epop Text
getStatusText =
  do statusBar <- findElem (ById "pageStatusBar")
     Just statusText <- attr statusBar "textContent"
     return statusText

expectInStatusBar :: String -> Epop ()
expectInStatusBar text =
  do statusText <- getStatusText
     unless (pack text `isInfixOf` statusText)
       (unexpected ("Couldn't find \"" ++ text ++ "\" in status bar\n" ++ unpack statusText))
       --TODO finish this refacto

-- TODO Fail gracefully when the page doesn't exist (before I arrived)
loadTimeSheetPage :: Day -> Epop ()
loadTimeSheetPage day =
  do setWindowSize (1600, 1600) -- Epop only loads what it needs to display, so display it all
     openEpop ("Timesheet.aspx?tsDate=" ++ displayedDay)
     waitUntil 10 (expectInStatusBar displayedDay)
     void uncheckPlanned
  where displayedDay = formatTime defaultTimeLocale "%d/%m/%Y" day

uncheckPlanned :: Epop Bool
uncheckPlanned =
  do optionsButton <- findElem (ById "Ribbon.ContextualTabs.TiedMode.Options-title")
     click optionsButton
     plannedCheckBox <- waitUntil 10
       (findElem (ById "Ribbon.ContextualTabs.TiedMode.Options.ShowHide.PlannedWork-Medium-checkbox"))
     checked <- isSelected plannedCheckBox
     if checked
       then debugSnap >> click plannedCheckBox >> liftIO (threadDelay 500000) >> return True
       else return False

-- | Returns True if the row of the table is yellow (and thus should be discarded)
isRowNotYellow :: Element -> Epop Bool
isRowNotYellow row = do cell <- findElemFrom row (ByXPath "td[2]")
                        style <- attr cell "style" !? "Could not find the style attribute of a table line"
                        return $ not ("background-color: rgb(255, 246, 193)" `isInfixOf` style)


getTasksFromTable :: Element -> Epop [Task]
getTasksFromTable taskTable =
  do headers <- findElemsFrom taskTable (ByXPath "tbody/tr/th") >>= mapM getText
     projectIndex <- elemIndex "Project Name" headers
       ?? "Could not find a column named \"Project Name\""
     nameIndex <- elemIndex "Task Name/Description" headers
       ?? "Could not find a column named \"Task Name/Description\""

     allRows <- findElemsFrom taskTable (ByXPath "tbody/tr")
     taskRows <- filterM isRowNotYellow . dropEnd 1 . drop 1 $ allRows
     forM taskRows $ \row ->
       do cells <- findElemsFrom row (ByXPath "td") >>= mapM getText
          let project = cells !! projectIndex
              name = cells !! nameIndex
          return (Task project name)


getDayCells :: [Day] -> Element -> Epop [[(Day, Element)]]
getDayCells days timeTable =
  do allRows <- findElemsFrom timeTable (ByXPath "tbody/tr")
     timeRows <- filterM isRowNotYellow . dropEnd 1 . drop 1 $ allRows
     forM timeRows $ \row ->
       do -- The first cell is the time type
          cells <- drop 1 <$> findElemsFrom row (ByTag "td")
          return (zip days cells)


data Table = Table [Task] [Day] [((Task, Day), Element)] deriving (Show)

parseTimeSheet :: Week -> Epop Table
parseTimeSheet week =
  do let firstDay = firstDayOfWeek week
         days = map (`addDays` firstDay) [0..4]
     loadTimeSheetPage firstDay

     magicId <- (findElem (ByName "WPClass:TimesheetPart") >>= flip attr "id") !? "Could not find magicId"
     taskTable <- findElem (ById (magicId ++ "_TimesheetPartJSGridControl_leftpane_mainTable"))
     timeTable <- findElem (ById (magicId ++ "_TimesheetPartJSGridControl_rightpane_mainTable"))

     tasks <- getTasksFromTable taskTable
     dayCells <- getDayCells days timeTable

     let table :: [((Task, Day), Element)]
         table = concatMap (\(task, dayCell) -> [((task, day), cell) | (day, cell) <- dayCell])
                           (zip tasks dayCells)
     return (Table tasks days table)


tasksByDay :: Table -> Epop [(Day, Maybe Task)]
tasksByDay (Table _ days table) = mapM taskForDay days
  where taskForDay day =
          do tableContent <- mapM (mapM getText) table  -- TODO we are doing this too often
             let tasksFilled = [t | ((t, d), v) <- tableContent, d == day, v == "1d"]
             task <- case tasksFilled  of
               [] -> return Nothing
               [t] -> return (Just t)
               _ -> throwE ("More than one tasks filled on " ++ show day)
             return (day, task)


setTasks :: [(Day, Maybe Task)] -> Epop ()
setTasks  = mapByWeek updateWeek fst
  where updateWeek :: (Week, [(Day, Maybe Task)]) -> Epop ()
        updateWeek (week, daysAndTasks) =
          do -- First, recall the page if needed
             loadTimeSheetPage (firstDayOfWeek week)
             statusText <- getStatusText
             when ("Your timesheet has been approved" `isInfixOf` statusText)
                  (recallWeek week)

             Table tasks days table <- parseTimeSheet week
             forM_ daysAndTasks $ \(day, mtask) ->
               do tryAssert "day not in table" (day `elem` days)
                  case mtask of
                    Just task -> tryAssert "task not in table" (task `elem` tasks)
                    Nothing -> return ()
                  forM_ table $ \((t, d), e) ->
                    when (d == day) (fillElem e (if mtask == Just t then "1d" else "0d"))
             -- TODO do not submit if not fully filled
             submitWeek

             -- Check
             fullTable <- parseTimeSheet week
             result <- tasksByDay fullTable
             tryAssert ("Error when filling week:\nWanted " ++ show daysAndTasks ++ "\nGot " ++ show result)
                       (all (`elem` result) daysAndTasks)

        fillElem e val =
          do click e
             click e
             sendKeys val e
             findElem (ById "suiteBarLeft") >>= click  -- Loose focus on cell
        submitWeek =
           do findElem (ById "Ribbon.ContextualTabs.TiedMode.Home-title")
                >>= flip findElemFrom (ByTag "a")
                >>= click
              submitButton <- waitUntil 10
                (findElem (ById "Ribbon.ContextualTabs.TiedMode.Home.Sheet.SubmitMenu-Large"))
              click submitButton
              turnInButton <- waitUntil 10
                (findElem (ById "Ribbon.ContextualTabs.TiedMode.Home.Sheet.SubmitTimesheet-Menu32"))
              click turnInButton
              waitUntil 10 (expectInStatusBar "Your timesheet has been approved")



debugSnap :: Epop ()
debugSnap = do s <- screenshot
               liftIO $ writeFile "/tmp/epop.png" s

getTasks :: [Day] -> Epop [(Day, Maybe Task)]
getTasks = mapByWeek tasksForWeek id
  where tasksForWeek :: (Week, [Day]) -> Epop [(Day, Maybe Task)]
        tasksForWeek (week, ds) =
          do table <- parseTimeSheet week
             allTasks <- tasksByDay table
             -- We only keep the one that the user asked
             return [(d, t) | (d, t) <- allTasks, d `elem` ds]


getAvailableTasks :: Day -> Epop [Task]
getAvailableTasks day =
  do Table tasks _ _ <- parseTimeSheet (weekOfDay day)
     return tasks

mapByWeek :: (Monad  m, Monoid a) => ((Week, [d]) -> m a) -> (d -> Day)-> [d] -> m a
mapByWeek task toDay days = concat <$> mapM task daysByWeek
  where daysByWeek = groupSort . map (weekOfDay . toDay &&& id) $ days


recallWeek :: Week -> Epop ()
recallWeek week =
  do weekLines <- listWeekLines
     weekLine <- findM isRightLine weekLines !? "Could not find week"
     click weekLine
     recallButton <- findElem (ById "Ribbon.ContextualTabs.MyTimesheets.Home.Timesheets.RecallTimesheet-Large")
     click recallButton
     _ <- waitUntil 5 expectAlertOpen
     acceptAlert
     liftIO $ threadDelay 400000
     -- TODO don't just wait, check the line says "In Progress"

  where isRightLine el =
          do Just weekText <- attr el "textContent"
             lineWeek <- parseWeek weekText
             return (lineWeek == week)

isLoggedIn :: Epop Bool
isLoggedIn = do setPageLoadTimeout 10000
                catchJust (\(FailedCommand typ _) -> if typ == ScriptTimeout then Just () else Nothing)
                          (openEpop "default.aspx" >> return True)
                          (\_ -> return False)
