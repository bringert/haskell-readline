-- Copyright (C) Anders Carlsson, Bjorn Bringert
module Readline (readline) where

import Control.Monad
import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)

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
-- * List with a cursor
--

type CursorList a = ([a],[a])



--
-- * Edit state
--

initState :: ReadState
initState = 
    ReadState { chars = ("",""), gotEOF = False }

{-# NOINLINE state #-}
state :: IORef ReadState
state = unsafePerformIO (newIORef initState)

clearState :: IO ()
clearState = writeIORef state initState

getState :: IO ReadState
getState = readIORef state

modifyState :: (ReadState -> ReadState) -> IO ()
modifyState f = modifyIORef state f

getChars :: IO (String,String)
getChars = liftM chars getState

modifyChars :: ((String,String) -> (String,String)) -> IO ()
modifyChars f = modifyState (\st -> st { chars = f (chars st) })

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
     ("\ESC[H", Move Home),
     ("\ESC[F", Move End),
     ("\DEL", DeletePrev),
     ("\ESC[3~", DeleteCurr),
     ("\ESC[A", HistoryPrev),
     ("\ESC[B", HistoryNext)
    ]
     
getCommand :: Commands -> IO Command
getCommand cs = 
    do c <- hGetChar stdin
       -- FIXME: remove
       appendFile "debug.log" (show c ++ "\n")
       let cs' = [(ss, command) | ((s:ss), command) <- cs, s == c]
       case cs' of 
		[] -> return $ Char c
		[("", command)] -> return command
		_ -> getCommand cs'


commandLoop :: IO ()
commandLoop =
    do command <- getCommand commands
       (xs,ys) <- getChars
       case command of
		    Move Previous | not (null xs) ->
			   do modifyChars $ \ (x:xs, ys) -> (xs, x:ys)
			      moveLeft 1
			      commandLoop
		    Move Next | not (null ys) ->
			   do modifyChars $ \ (xs, y:ys) -> (y:xs, ys)
			      moveRight 1
			      commandLoop
		    Move Home ->
			   do modifyChars $ \ (xs, ys) -> ("", reverse xs ++ ys)
			      moveLeft (length xs)
			      commandLoop
		    Move End ->
			   do modifyChars $ \ (xs, ys) -> (xs ++ ys, "")
			      moveRight (length ys)
			      commandLoop
		    Char c -> 
			   do modifyChars $ \ (xs, ys) -> (c:xs, ys)
			      putStr (c:ys)
			      moveLeft (length ys)
			      commandLoop
		    DeletePrev | not (null xs) ->
			   do modifyChars $ \ (_:xs, ys) -> (xs, ys)
			      moveLeft 1
			      let ys' = ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      commandLoop
		    DeleteCurr | not (null ys) ->
			   do modifyChars $ \ (xs, _:ys) -> (xs, ys)
			      let ys' = drop 1 ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      commandLoop
		    Accept -> putStrLn ""
		    _ -> commandLoop

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
    do hPutStr stdout prompt
       hFlush stdout
       withNoBuffOrEcho commandLoop
       (xs,ys) <- getChars
       clearState
       return $ Just $ reverse xs ++ ys



							    
