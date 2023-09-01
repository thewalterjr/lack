import Data.List
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering, LineBuffering))
import GHC.IO.FD (stdout)
import System.IO
    ( hSetBuffering, stdout, BufferMode(LineBuffering, NoBuffering) )

data Task = Task
    {
        taskId :: Int, 
        taskDescription :: String,
        inProgress :: String
    } deriving (Eq, Show)

type TodoList = [Task]

clearScreen :: IO ()
clearScreen = do
    hSetBuffering System.IO.stdout NoBuffering
    putStr "\ESC[2J"
    hSetBuffering System.IO.stdout LineBuffering
    putStr "\ESC[H"

addTask :: TodoList -> String -> TodoList
addTask todoList task = todoList ++ [Task(length todoList + 1) task ""]

removeTask :: Int -> TodoList -> TodoList
removeTask id list = filter(\task -> taskId task /= id) list

displayTasks :: TodoList -> IO ()
displayTasks list = putStrLn $ unlines $ map (\task -> show (taskId task) ++ ". " ++ taskDescription task ++ " " ++ inProgress task) list

updateInProgress :: Int -> TodoList -> TodoList
updateInProgress id list = map updateTask list
  where
    updateTask task
      | taskId task == id = task { inProgress = "*DOING*" }
      | otherwise = task


todoLoop :: TodoList -> IO()
todoLoop list = do
    putStrLn "\n\x1b[1mTODO LIST\x1b[0m\n"
    displayTasks list
    putStrLn "1. \x1b[32mAdd task\x1b[0m"
    putStrLn "2. \x1b[33mAdd priority\x1b[0m"
    putStrLn "3. \x1b[31mRemove task\x1b[0m"
    putStrLn "Select an option:"
    option <- getLine
    case option of
        "1" -> do 
            putStrLn "Enter task description:"
            description <- getLine
            let newList = addTask list description
            clearScreen
            todoLoop newList
        "2" -> do
            putStrLn "Enter task number to put in doing"
            id <- getLine 
            let taskId = read id :: Int
            let newList = updateInProgress taskId list
            clearScreen
            todoLoop newList
        "3" -> do
            putStrLn "Enter task number to remove:"
            id <- getLine 
            let taskId = read id :: Int
            let newList = removeTask taskId list
            clearScreen
            todoLoop newList

    
main :: IO()
main = do
    todoLoop []
