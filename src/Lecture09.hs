{-# LANGUAGE DuplicateRecordFields #-}


module Lecture09 where

import System.Directory
import System.IO
import System.FilePath
import System.Random
import Data.List

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read, Ord)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

instance Ord Todo where
  compare (Todo _ _ _ deadline1 _) (Todo _ _ _ deadline2 _) = compare deadline1 deadline2

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

-- Создать список Todo
createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  _ <- createDirectoryIfMissing True rootFolder
  return (TodoList rootFolder)

-- Добавить новую заметку
addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo (TodoList todoListPath) title content deadline = do
  (fileName, file) <- openTempFile todoListPath "tmp" 
  let
    todoId = Id (takeFileName fileName)
  hPutStr file (show (Todo todoId title content deadline False))
  hClose file
  return todoId

-- Прочитать Todo
readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList todoListPath) (Id todoId) = do
  todo <- readFile (joinPath [todoListPath, todoId])
  return (read todo)

-- Показать Todo
showTodo :: TodoList -> Id -> IO ()
showTodo (TodoList todoListPath) (Id todoId) = do
  todo <- readFile (joinPath [todoListPath, todoId])
  putStrLn todo

-- Удалить Todo
removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList todoListPath) (Id todoId) = removeFile (joinPath [todoListPath, todoId])

-- вставить в editTodo, setTodoAsDone делать через editTodo
replaceTodo :: TodoList -> Id -> Todo -> IO ()
replaceTodo todoList@(TodoList todoListPath) (Id todoId) todo = do
  (tmpName, tmpFile) <- openTempFile todoListPath "tmp"
  hPutStr tmpFile (show todo)
  hClose tmpFile 
  let
    filePath = joinPath [todoListPath, todoId]
  removeFile filePath
  renameFile tmpName filePath

-- Отредактировать Todo
editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList todoId (TodoEdit title content deadline) = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo {
      title = title,
      content = content,
      deadline = deadline
    }

-- Установить Todo как выполненное
setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList todoId = do
  todo <- readTodo todoList todoId
  replaceTodo todoList todoId todo { isDone = True }

-- Todo должны быть упорядочены по возрастанию deadline'а
-- Прочитать все Todo
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootFolder) = do
  files <- listDirectory rootFolder
  todo <- mapM (readTodo todoList . Id) files
  return (sort todo)

-- Прочитать неоконченные Todo
readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  allTodo <- readAllTodo todoList
  return (filter (\(Todo _ _ _ _ isDone) -> not isDone) allTodo)

-- Показать массив Todo
showTodos :: [Todo] -> IO ()
showTodos = mapM_ (putStrLn . show)

-- Показать все Todo из списка
showAllTodo :: TodoList -> IO ()
showAllTodo todoList = readAllTodo todoList >>= showTodos

-- Показать невыполненные Todo
showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = readUnfinishedTodo todoList >>= showTodos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  _              <- putStrLn "playGuessGame"
  numberForGuess <- randomRIO (0, 100)
  guessNumber numberForGuess

guessNumber :: Int -> IO ()
guessNumber numberForGuess = do
  _          <- putStrLn "You number: "
  userNumber <- readLn
  case (compare userNumber numberForGuess) of
    LT -> do
      putStrLn "Too small"
      guessNumber numberForGuess
    GT -> do
      putStrLn "Too big"
      guessNumber numberForGuess
    EQ -> do
      putStrLn "Yep, that's the number!"


-- </Задачи для самостоятельного решения>