-- Copyright (C) Anders Carlsson, Bjorn Bringert
module Readline (readline, addHistory) where

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
		historyState :: ([String],[String]),
		gotEOF :: Bool }



debug = appendFile "debug.log"

--
-- * Utilities
--

atLeast :: Int -> [a] -> Bool
atLeast n _ | n <= 0 = True
atLeast n [] = False
atLeast n (x:xs) = atLeast (n-1) xs

--
-- * List with a cursor
--

type CursorList a = ([a],[a])

mkCursorList :: [a] -> CursorList a
mkCursorList xs = (reverse xs,[])

toList :: CursorList a -> [a]
toList (xs,ys) = reverse xs ++ ys

previous :: CursorList a -> CursorList a
previous (x:xs,ys) = (xs,x:ys)

next :: CursorList a -> CursorList a
next (xs,y:ys) = (y:xs,ys)

toStart :: CursorList a -> CursorList a
toStart l = ([], toList l)

toEnd :: CursorList a -> CursorList a
toEnd (xs,ys) = (xs ++ reverse ys, [])

insert :: a -> CursorList a -> CursorList a
insert x (xs,ys) = (x:xs, ys)

set :: [a] -> CursorList a -> CursorList a
set xs _ = mkCursorList xs

setNext :: a -> CursorList a -> CursorList a
setNext x (xs,_:ys) = (xs, x:ys)

--
-- * Edit state
--

initState :: IO ReadState
initState = 
    do
    h <- getHistory
    return ReadState { chars = mkCursorList "", 
		       historyState = (h,[""]),
		       gotEOF = False }

modifyChars :: ReadState -> ((String,String) -> (String,String)) -> ReadState
modifyChars st@(ReadState{ chars = cs}) f = st{ chars = f cs }

modifyHistoryState :: ReadState -> (([String],[String]) -> ([String],[String])) -> ReadState
modifyHistoryState st@(ReadState{ historyState = hs }) f = st { historyState = f hs }

--
-- * History
--

type History = [String]

{-# NOINLINE history #-}
history :: IORef History
history = unsafePerformIO (newIORef initHistory)

initHistory :: History
initHistory = []

addHistory :: String -> IO ()
addHistory str = modifyIORef history (str:)

getHistory :: IO History
getHistory = readIORef history

--
-- * Cursor movement
--

prevStr = "\ESC[D"
nextStr = "\ESC[C"

moveLeft :: Int -> IO ()
moveLeft n = putStr $ concat $ replicate n prevStr

moveRight :: Int -> IO ()
moveRight n = putStr $ concat $ replicate n nextStr

replaceLine :: String -> String -> IO ()
replaceLine old new = 
    do moveLeft (length old)
       let sp = replicate (length old - length new) ' '
       putStr (new ++ sp)
       moveLeft (length sp)

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
       debug (show c ++ "\n")
       let cs' = [(ss, command) | ((s:ss), command) <- cs, s == c]
       case cs' of 
		[] -> return $ Char c
		[("", command)] -> return command
		_ -> getCommand cs'


commandLoop :: ReadState -> IO ReadState
commandLoop st@(ReadState{chars = cs@(xs,ys), historyState = (h1,h2) }) =
    do command <- getCommand commands
       debug (show (historyState st) ++ "\n")
       case command of
		    Move Previous | not (null xs) ->
			   do moveLeft 1
			      loop previous
		    Move Next | not (null ys) ->
			   do moveRight 1
			      loop next
		    Move Home ->
			   do moveLeft (length xs)
			      loop toStart
		    Move End ->
			   do moveRight (length ys)
			      loop toEnd
		    Char c -> 
			   do putStr (c:ys)
			      moveLeft (length ys)
			      loop $ insert c
		    DeletePrev | not (null xs) ->
			   do moveLeft 1
			      let ys' = ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      loop $ \ (_:xs, ys) -> (xs, ys)
		    DeleteCurr | not (null ys) ->
			   do let ys' = drop 1 ys ++ " "
			      putStr ys'
			      moveLeft (length ys')
			      loop $ \ (xs, _:ys) -> (xs, ys)
		    HistoryPrev | not (null h1) ->
			   do let h = head h1
			      replaceLine xs h
			      loopHistory (set h) (previous . setNext (toList cs))
		    HistoryNext | atLeast 2 h2 ->
			   do let _:h:_ = h2
			      replaceLine xs h
			      loopHistory (set h) (next . setNext (toList cs))
		    Accept -> 
			   do putStrLn ""
			      return st
		    _ -> do commandLoop st
	   where loop = commandLoop . modifyChars st
		 loopHistory cf hf = commandLoop $ modifyChars (modifyHistoryState st hf) cf

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
       st <- initState
       st' <- withNoBuffOrEcho (commandLoop st)
       return $ Just $ toList $ chars st'



							    
