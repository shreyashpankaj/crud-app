{-# OPTIONS_GHC -Wall #-}

module Exam where
-- Rules:
--
-- - You can only import modules from the base package (but that is allowed).
--
-- - You can decide (unless explicitly specified) whether to implement a function using
--   existing (higher-order) functions or directly via pattern matching and recursion.
--   However, if you decide to use folds, then make sure you are making a sensible
--   choice between the different kinds of folds. And if you don't, still pay attention
--   as to whether the function should be incremental, or whether the use of an
--   accumulator is adequate.
--
-- - In any case, try to use good style. Easy to read code, consistent indentation and
--   spacing, consistent identifier names, not too many redundant parentheses, no overly
--   complicated expressions. I will deduct points for otherwise correct
--   solutions if they are over-complicated, difficult to understand or read, or
--   needlessly inefficient.
--
-- - All tasks give the same number of points, but they do not have the same degree of
--   difficulty. The tasks are ordered roughly in increasing order of difficulty (as
--   judged by me).
--
-- - None of the (regular) tasks build on each other, so if you do not manage to solve
--   one, you can still try the next.
--
-- - Make sure that your program loads successfully into GHCi without any syntax or type
--   errors prior to submission!
--
-- - Submit within two hours via email to <exams@well-typed.com> in reply to
--   email with the exam. Attach the Haskell source file with your solutions,
--   and *only* this file. Please use the reply-to address.
--
-- - There are two bonus tasks you can do if you have time left and want to show off
--   how good you are. (In contrast to the normal tasks, the bonus tasks may reuse
--   elements from or build on earlier tasks.)

import Data.List
import Control.Monad


-- Task 1. (5 points)
--
-- Define a function
--
--   positivePrefix :: [Int] -> [Int]
--
-- that returns the longest prefix of the given list containing
-- only positive numbers.
--
-- Examples:
--
-- >>> positivePrefix [1,2,3,0,3]
-- [1,2,3]
-- >>> positivePrefix [-1,1,-2,2]
-- []
-- >>> positivePrefix [5,4,3,2,1]
-- [5,4,3,2,1]
--
positivePrefix :: [Int] -> [Int]
-- positivePrefix = error "TODO: implement this"
positivePrefix [] = []
positivePrefix (x : xs)
    | x > 0 = x : positivePrefix xs
    | otherwise = []



-- Task 2. (5 points)
--
-- Define a function
--
--   twoSmallest
--
-- that computes a pair of the two smallest elements of a list,
-- in ascending order. The function should return a Maybe value,
-- and it should return Nothing if the list contains fewer than
-- two elements.
--
-- Add a suitable type signature yourself!
--
-- Examples:
--
-- >>> twoSmallest []
-- Nothing
-- >>> twoSmallest "x"
-- Nothing
-- >>> twoSmallest "hello"
-- Just ('e', 'h')
-- >>> twoSmallest [1,4,7,3,-4,5,9]
-- Just (-4,1)
-- >>> twoSmallest [4,6,2,2,3,5]
-- Just (2,2)
--

-- twoSmallest = error "TODO: implement this, AND ADD A SUITABLE TYPE SIGNATURE"
twoSmallest :: Ord a => [a] -> Maybe (a, a)
twoSmallest xs
  | length xs < 2 = Nothing
  | otherwise = Just (min1, min2)
  where
    min1 = minimum xs
    rest = delete min1 xs
    min2 = minimum rest



-- Task 3. (5 points)
--
-- The following type of "snoc-lists" represents lists that
-- grow to the right rather than to the left.
--
-- ("snoc" is the word "cons" read from right to left.)
--
-- Define a function
--
--   rev :: SnocList a -> [a]
--
-- that reverses a snoc-list by turning it into a list.
--
-- Examples:
--
-- >>> rev (Lin :- 1)
-- [1]
-- >>> rev (Lin :- 'c' :- 'b' :- 'a')
-- "abc"
-- >>> rev (Lin :- False :- True)
-- [True, False]
-- >>> rev Lin
-- []

data SnocList a = Lin | SnocList a :- a
  deriving (Show)

infixl 5 :-

rev :: SnocList a -> [a]
-- rev = error "TODO: implement this"
rev Lin = []
rev (xs :- x) = x : rev xs

-- Task 4. (5 points)
--
-- Define a function
--
--   getNonEmptyLines :: Int -> IO [String]
--
-- that defines an IO action that when executed, reads the
-- given number of lines from the user, but returns only such
-- lines that are not empty (you do not have to check for
-- whitespace, just for being completely empty).
--
-- If the given number is negative, no lines should be read
-- and returned.
--
-- In the example below, lines entered by the user during the execution
-- of the IO action are annotated with the string "<-- user input",
-- which itself is not part of the input.
--
-- GHCi> getNonEmptyLines 0
-- []
-- GHCi> getNonEmptyLines 2
-- foo      <-- user input
-- bar      <-- user input
-- ["foo,"bar"]
-- GHCi> getNonEmptyLines 1
--          <-- user input
-- []
-- GHCi> getNonEmptyLines 3
-- foo      <-- user input
--
-- bar      <-- user input
-- ["foo","bar"]
-- GHCi> getNonEmptyLines (-2)
-- []
--
--
getNonEmptyLines :: Int -> IO [String]
-- getNonEmptyLines = error "TODO: implement this"
getNonEmptyLines n 
    | n <= 0 = return []
    | otherwise = do
        input <- replicateM n getLine
        return (filter (not.null) input)
        



-- Task 5. (5 points)
--
-- Write a function
--
--   simpleDiff :: FilePath -> FilePath -> IO DiffResult
--
-- that checks whether the two given files have the same contents.
-- It is ok to assume that these are text files and to use readFile
-- for reading them. If any of the given files do not exist or are
-- unreadable, the function may crash.
--
-- Examples:
--
-- >>> writeFile "test1.txt" "abc"
-- >>> writeFile "test2.txt" "abd"
-- >>> writeFile "test3.txt" " ab\nc"
-- >>> writeFile "test4.txt" "abc"
-- >>> writeFile "test5.txt" " ab\nc"
-- >>> simpleDiff "test1.txt" "test2.txt"
-- Different
-- >>> simpleDiff "test1.txt" "test3.txt"
-- Different
-- >>> simpleDiff "test1.txt" "test4.txt"
-- Same
-- >>> simpleDiff "test3.txt" "test5.txt"
-- Same
--

data DiffResult = Same | Different
  deriving (Show, Eq)

simpleDiff :: FilePath -> FilePath -> IO DiffResult
-- simpleDiff = error "TODO: implement this"
simpleDiff f1 f2 = do 
    text1 <- readFile f1
    text2 <- readFile f2 
    return $ if text1 == text2 
            then Same 
            else Different




-- Task 6. (5 points)
--
-- Define a function getLinesWhile that uses the function getLine
-- to read lines from the user as long as the strings entered
-- satisfy the given condition. Once the user enters a string
-- that does *not* satisfy the given condition, the lines read
-- so far should, in the original order, be returned by the action.
--
-- Examples (user input is marked with a * on the right,
-- but the * is not part of the input):
-- >>> getLinesWhile (not . null)
-- foo                               *
-- bar                               *
--                                   *
-- ["foo","bar"]
-- >>> getLinesWhile (all (== 'x'))
--                                   *
-- x                                 *
-- xxx                               *
-- X                                 *
-- ["","x","xxx"]
-- >>> getLinesWhile (all (== 'x'))
-- xxy                               *
-- []
--

getLinesWhile :: (String -> Bool) -> IO [String]
-- getLinesWhile = error "TODO: implement this"
getLinesWhile cond = do
    input <- getLine
    if cond input 
        then do 
            rest <- getLinesWhile cond
            return (input : rest)
        else 
            return []



-- BONUS Task 7. (+5 points)
--
-- Using a pair of a snoc-list and a normal list, one can
-- model a cursor within a list, i.e., identify a position
-- within the list in such a way that one can efficiently move
-- left or right.
--
-- The snoc-list corresponds to the part of the list to the
-- left of the current position, and the list to the part that
-- is to the right of the current position.
--
-- So for example, given the list [1,2,3,4], if we want to
-- have a cursor between the 2 and the 3, we would represent
-- it as
--
--   MkCursor (Lin :- 1 :- 2) (3 : 4 : [])
--
-- If the snoc-list is empty, this represents a position at the
-- very beginning of the list. If the normal list is empty, the
-- cursor is at the very end of the original list.
--
-- Functions 'fromList' that turn a list into a cursor positioned
-- at the beginning, and 'at' performing a lookup of the element
-- immediately following the cursor position, are given.
--
-- Implement the functions
--
--   left         :: Cursor a -> Cursor a
--   right        :: Cursor a -> Cursor a
--   cursorToList :: Cursor a -> [a]
--
-- where the functions 'left' and 'right' should move the cursor one
-- position to the left or to the right, respectively. These functions
-- leave the cursor unchanged if such a move is not possible because
-- the cursor is already at the corresponding end of the list.
--
-- The function 'cursorToList' should recover the original list contents.
-- (Hint: you can use 'left' to help with this.)
--
-- Examples:
--
-- >>> at (cursorFromList [1,2,3])
-- Just 1
-- >>> at (right (cursorFromList [1,2,3]))
-- Just 2
-- >>> at (left (cursorFromList [1,2,3]))
-- Just 1
-- >>> at (right (left (right (right (cursorFromList [1,2,3])))))
-- Just 3
-- >>> at (right (right (right (left (cursorFromList [1,2,3])))))
-- Nothing
-- >>> at (left (right (right (left (cursorFromList [1,2,3])))))
-- Just 2
-- >>> at (left (right (right (right (left (cursorFromList [1,2,3]))))))
-- Just 3
-- >>> at (left (right (right (right (right (left (cursorFromList [1,2,3])))))))
-- Just 3
-- >>> cursorToList (left (right (right (right (left (cursorFromList [1,2,3]))))))
-- [1,2,3]
--
-- Note that
--
--   left . right
--
-- moves a cursor right and then left.
--

data Cursor a = MkCursor (SnocList a) [a]

cursorFromList :: [a] -> Cursor a
cursorFromList xs = MkCursor Lin xs

at :: Cursor a -> Maybe a
at (MkCursor _pre [])          = Nothing
at (MkCursor _pre (x : _post)) = Just x

left :: Cursor a -> Cursor a
left = error "TODO: implement this"

right :: Cursor a -> Cursor a
right = error "TODO: implement this"

cursorToList :: Cursor a -> [a]
cursorToList = error "TODO: implement this"



-- BONUS Task 8. (+5 points)
--
-- We want to process a list of steps. A step can be Begin, Wait, or End.
-- This is given by the Step datatype below.
--
-- We want to validate that the list of steps is well-ordered and determine
-- the number of Waits between each group that starts with a Begin and ends
-- with an End.
--
-- Waits that occur outside of a Begin and End are simply discarded.
--
-- Groups cannot be nested. If an End occurs without a Begin, or a Begin
-- occurs while already in a group, this results in a failure *at that point*,
-- and the rest of the list is not processed anymore. Also, if the list of
-- steps ends while within a group, this results in a failure.
--
-- To capture results, there is a Result datatype given below. A Result
-- is effectively a list of groups with a certain length, but there is a
-- marker for a successful end (Done) and a failure (Fail).
--
-- Examples:
--
-- >>> process []
-- Done
-- >>> process [Begin, End]
-- Group 0 Done
-- >>> process [Begin, Wait, End]
-- Group 1 Done
-- >>> process [Wait, Begin, Wait, Wait, End, Wait, Wait, Wait]
-- Group 2 Done
-- >>> process [Wait, Begin, Wait, Wait, End, Wait, Begin, Wait, Wait, Wait, End, Begin, End]
-- Group 2 (Group 3 (Group 0 Done))
-- >>> process [Begin, Begin, End, End]
-- Fail
-- >>> process [Wait, Wait, Wait]
-- Done
-- >>> process [Begin, End, Wait, Wait, Wait, End]
-- Group 0 Fail
-- >>> process [Begin, End, Begin, Wait, Wait, Wait, End, Begin, Wait, Wait, Begin, End]
-- Group 0 (Group 3 Fail)
--

data Step =
    Begin
  | Wait
  | End
  deriving Show

data Result =
    Group Int Result
  | Done
  | Fail
  deriving Show

process :: [Step] -> Result
process = error "TODO: implement this"
