-- Copyright (C) Anders Carlsson, Bjorn Bringert
module Readline (readline) where

import System.IO
import Data.IORef
import Control.Monad

data Command 
    = Move Cursor
    | Accept
    | DeletePrev
    | DeleteCurr
    | Char Char
    | HistoryPrev
    | HistoryNext
    deriving Show

data Cursor 
    = Previous
    | Next
    | Home
    | End
    deriving Show

type Commands = 
    [(String, Command)]

data ReadState = 
    ReadState { chars :: (String, String),
		gotEOF :: Bool }

--
-- * Edit state
--

initState :: ReadState
initState = 
    ReadState { chars = ("",""), gotEOF = False }

atBeginning :: IORef ReadState -> IO Bool
atBeginning stateRef = liftM (null . fst . chars) $ readIORef stateRef

atEnd :: IORef ReadState -> IO Bool
atEnd stateRef = liftM (null . snd . chars) $ readIORef stateRef

getChars :: IORef ReadState -> IO (String,String)
getChars = liftM chars . readIORef

modifyChars ::  IORef ReadState 
	    -> ((String,String) -> (String,String)) -> IO ()
modifyChars r f = modifyIORef r (\st -> st { chars = f (chars st) })

--
-- * Cursor movement
--

prevStr = "\ESC[D"
nextStr = "\ESC[C"

moveLeft :: Int -> IO ()
moveLeft n = putStr $ concat $ replicate n prevStr

moveRight :: Int -> IO ()
moveRight n = putStr $ concat $ replicate n nextStr

--
-- * Input to Command
--

commands :: Commands
commands = 
    [("\n", Accept),
     (prevStr, Move Previous),
     (nextStr, Move Next),
     ("\SOH", Move Home),
     ("\ENQ", Move End),
     ("\DEL", DeletePrev),
     ("\ESC[3~", DeleteCurr),
     ("\ESC[A", HistoryPrev),
     ("\ESC[B", HistoryNext)
    ]
     
getCommand :: Commands -> IO Command
getCommand cs = 
    do c <- hGetChar stdin
       -- FIXME: remove
       appendFile "debug.log" (show c)
       let cs' = [(ss, command) | ((s:ss), command) <- cs, s == c]
       case cs' of 
		[] -> return $ Char c
		[("", command)] -> return command
		_ -> getCommand cs'


commandLoop :: IORef ReadState -> IO ()
commandLoop stateRef =
    do command <- getCommand commands
       (xs,ys) <- liftM chars $ readIORef stateRef
       case command of
		    Move Previous | not (null xs) ->
			   do mc $ \ (x:xs, ys) -> (xs, x:ys)
			      moveLeft 1
			      continue
		    Move Next | not (null ys) ->
			   do mc $ \ (xs, y:ys) -> (y:xs, ys)
			      moveRight 1
			      continue
		    Move Home ->
			   do mc $ \ (xs, ys) -> ("", reverse xs ++ ys)
			      moveLeft (length xs)
			      continue
		    Move End ->
			   do mc $ \ (xs, ys) -> (xs ++ ys, "")
			      moveRight (length ys)
			      continue
		    Char c -> 
			   do mc $ \ (xs, ys) -> (c:xs, ys)
			      putStr (c:ys)
			      moveLeft (length ys)
			      continue
		    DeletePrev | not (null xs) ->
			   do mc $ \ (_:xs, ys) -> (xs, ys)
			      moveLeft 1
			      let ys' = ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      continue
		    DeleteCurr | not (null ys) ->
			   do mc $ \ (xs, _:ys) -> (xs, ys)
			      let ys' = drop 1 ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      continue
		    Accept -> putStrLn ""
		    _ -> continue
       where mc = modifyChars stateRef
	     continue = commandLoop stateRef

withNoBuffOrEcho :: IO a -> IO a
withNoBuffOrEcho m = 
    do
    oldInBuf <- hGetBuffering stdin
    oldOutBuf <- hGetBuffering stdout
    oldEcho <- hGetEcho stdout
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdout False
    x <- m
    hSetBuffering stdin oldInBuf
    hSetBuffering stdout oldOutBuf
    hSetEcho stdout oldEcho
    return x

-- FIXME: restore terminal settings if interrupted
readline :: String -> IO (Maybe String)	    
readline prompt =
    do stateRef <- newIORef initState
       hPutStr stdout prompt
       hFlush stdout
       withNoBuffOrEcho (commandLoop stateRef)
       (xs,ys) <- getChars stateRef
       return $ Just $ reverse xs ++ ys



							    
