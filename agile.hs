-- The MIT License (MIT)
-- 
-- Copyright (c) 2013 Jonathan McCluskey 
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.


-- The agile project management program is intended to allow developers to have
-- a command-line interface to a text-based agile storage database.  This database
-- can be initialized and stored along with the source and version controlled
-- along with the source

import System.Environment   
import System.Directory  
import System.IO  
import System.IO.Error
import Data.List  

-- 
-- The main function
--
main = argumentsParser `catch` exceptionHandler

-- 
-- Command-line argument parser
--
argumentsParser :: IO ()
argumentsParser = do  
    (command:args) <- getArgs  
    handleCommand command args 

--
-- Handler for the first command-line argument
--
handleCommand :: String -> [String] -> IO ()
handleCommand command args
    | command == "init"    = initialize 
    | command == "status"  = status 
    | command == "roadmap" = roadmap args 
    | command == "feature" = feature args 
    | command == "story"   = story args 
    | command == "task"    = task args 
    | otherwise            = usage

--
-- Initializes the text-based agile storage database
--
initialize :: IO () 
initialize = do
    createDirectoryIfMissing False ".agile" 
    appendFile ".agile/roadmap" ("")
    appendFile ".agile/features" ("")
    appendFile ".agile/stories" ("")
    appendFile ".agile/tasks" ("")
    putStrLn "Initialized."

-- 
-- Displays the status of the project
--
status :: IO ()
status = do
    putStrLn "Status does nothing yet."

-- 
-- Allows for adding, removing, and showing roadmap items
--
roadmap :: [String] -> IO ()
roadmap args = do
    putStrLn "Roadmap does nothing yet."

-- 
-- Allows for adding, removing, and showing features
--
feature :: [String] -> IO ()
feature args = do
    putStrLn "Feature does nothing yet."

-- 
-- Allows for adding, removing, and showing stories
--
story :: [String] -> IO ()
story args = do
    putStrLn "Story does nothing yet."

-- 
-- Allows for adding, removing, and showing tasks
--
task :: [String] -> IO ()
task []            = usage
task (arg:args) 
    | arg == "add"    = taskAdd args
    | arg == "rm"     = taskRm args
    | arg == "show"   = taskShow 
    | otherwise       = usage 

--
-- Adds a task to the agile storage database
--
taskAdd :: [String] -> IO ()
taskAdd [] = usage
taskAdd [todoItem]= do
    appendFile ".agile/tasks" (todoItem ++ "\n")
    putStrLn "Task added."

--
-- Removes a task to the agile storage database
--
taskRm :: [String] -> IO ()
taskRm [] = usage
taskRm [numberString] = do
    handle <- openFile ".agile/tasks" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile ".agile/tasks"  
    renameFile tempName ".agile/tasks"
    putStrLn "Task removed."

-- 
-- Shows the tasks in the agile storage database
--
taskShow :: IO ()
taskShow = do 
    contents <- readFile ".agile/tasks"  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

--
-- Exception Handler
--
exceptionHandler :: IOError -> IO ()
exceptionHandler e = usage 

-- 
-- Displays the usage statement
--
usage :: IO ()
usage = do
    putStrLn "USAGE: agile command [options]"
    putStrLn "  command:"
    putStrLn "    init"
    putStrLn "    status"
    putStrLn "    roadmap"
    putStrLn "    feature"
    putStrLn "    story"
    putStrLn "    task"
    putStrLn "  options:"
    putStrLn "    add [map=1,2,3] [date=YYYY/MM/DD] [phase=[new|planned|current|impeded|closed]] [title=\"Title\"] [body=\"Body\"] "
    putStrLn "    update [number] [map=1,2,3] [map+=4] [map-=2] [date=YYYY/MM/DD] [phase=[new|planned|current|impeded|closed]] [title=\"Title\"] [body=\"Body\"] "
    putStrLn "    rm [number]"
    putStrLn "    show"
    putStrLn ""

