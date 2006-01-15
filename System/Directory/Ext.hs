-- | Some utility functions for path names not included in System.Directory.
module System.Directory.Ext (pathSeparator, pathJoin, resolvePath, getDirFile) where

import Data.List (intersperse, isPrefixOf, tails, findIndex)
import Data.Maybe (fromJust)


pathSeparator :: String
pathSeparator = "/"     -- FIXME: #ifdef for win32 etc.

-- | Create a path from a list of path elements
pathJoin :: [String] -> FilePath
pathJoin = concat . intersperse pathSeparator


-- | Make a path absolute if it is not absolute already.
--   NOTE: well, if the base path is not absolute, the result
--   is not guaranteed to be absolute. 
resolvePath :: FilePath -- ^ Absolute base path
	       -> FilePath -- ^ Possibly relative path
	       -> FilePath -- ^ Absolute path
resolvePath base p 
    | isAbsolute p = p
    | otherwise = pathJoin [base,p]

-- | Check if a path name is absolute.
isAbsolute :: FilePath -> Bool
isAbsolute p = pathSeparator `isPrefixOf` p -- FIXME: unix-specific



-- | Split a file path into a directory name and a file name
getDirFile :: FilePath -> (FilePath, String)
getDirFile = splitAtLast pathSeparator


{-
-- | Get the pathname prefix, if there is one. Returns the empty
--   string if not.
getPrefix :: FilePath -> String
getPrefix = fst . getPrefixPath

-- Split a pathname into a prefix and a relative path
getPrefixPath :: FilePath -> (String, String) 
-- FIXME: implementation is unix-specific
getPrefixPath p 
    | pathSeparator `isPrefixOf` p = (pathSeparator, dropWhileList pathSeparator p)
    | otherwise = ("", p)

-- | Get the directory component of a path (i.e. everything but the
--   last path component).
--
-- POSIX examples:
-- > getDirectory "" ==> "."
-- > getDirectory "foo" ==> "."
-- > getDirectory "home/foo" ==> "home"
-- > getDirectory "home/foo/bar" ==> "home/foo"
-- > getDirectory "/" ==> "/"
-- > getDirectory "/home" ==> "/"
-- > getDirectory "/home/" ==> "/"
-- > getDirectory "/home/foo" ==> "/home"
-- > getDirectory "/home/foo/bar" ==> "/home/foo"
getDirectory :: FilePath -> FilePath
getDirectory p = case pref ++ l of { "" -> "."; x -> x }
    where 
    rps = reverse pathSeparator
    (pref, path) = getPrefixPath p
    -- reverse path and get rid of trailing path separators
    path' = dropWhileList rps $ reverse path
    l = reverse $ dropWhileList rps $ dropUntilList rps path'


-- | Get the filename component of a path 
--   (i.e. the last component of the path).
--   Returns the empty string if there is no file name component.
getFilename :: FilePath -> String
getFilename = safeLast "" . pathSplit

-- | Split a path into a list of path elements
pathSplit :: FilePath -> [String]
pathSplit = split pathSeparator

-}

--
-- Tuple utilities
--

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

flipPair :: (a, b) -> (b, a)
flipPair (x, y) = (y, x)

--
-- List utilities
--

-- | Split a list at the last occurrence of a sublist. If the sublist
--   does not exist in the input, the entire input will be returned
--   in the second component of the result.
splitAtLast :: Eq a => 
	       [a]       -- ^ List to split at
	    -> [a]       -- ^ List to split
	    -> ([a],[a])
splitAtLast xs ys = 
    case findIndexList (reverse xs) ys' of
         Just i -> flipPair $ both reverse $ splitAt i ys'
	 Nothing -> ([], ys)
    where 
    ys' = reverse ys
    i = findIndexList (reverse xs) ys'

findIndexList :: Eq a => [a] -> [a] -> Maybe Int
findIndexList xs = findIndex (xs `isPrefixOf`) . tails

{-

-- | Like 'init', but returns the empty list when given the empty list.
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

-- | Like last, but with a default value to return when given the empty list.
safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ xs = last xs

-- | Drop elements until the given list is the prefix of the result.
--   Returns the empty list of the prefix is not in the input.
dropUntilList :: Eq a => [a] -> [a] -> [a]
dropUntilList xs ys = 
    case filter (xs `isPrefixOf`) (tails ys) of
         [] -> []
	 (x:_) -> x

-- | Repeatedly drop the given lsit from the input, until it is not
--   a prefix of the result.
dropWhileList :: Eq a => [a] -> [a] -> [a]
dropWhileList p = f
    where
    f ys | p `isPrefixOf` ys = f (drop (length p) ys)
    f ys = ys

---------------------------------------------------------------------------------
-- Tom Moertel - from lambdabot's Util module

-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]
split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces

split glue xs = split' xs
    where
    split' [] = []
    split' xs | rest == glue = piece : [[]]
	      | otherwise = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs
    dropGlue = drop (length glue)

-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")
breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)

breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest') where (piece, rest') = breakOnGlue glue xs

-}