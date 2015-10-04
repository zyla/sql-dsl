{-# LANGUAGE TupleSections #-}
-- | Supply commands to GHCi and compare its output with expected.
module Main where

import Control.Applicative
import Data.Maybe

import System.Process
import System.Exit
import System.Environment (getArgs)

data Test = Test
    { testCommand :: String
    , testExpectation :: Maybe Expectation
    }

type Expectation = String
type Err = String

readTests :: String -> [Test]
readTests = map blockToTest . blocks . lines'
  where
    blockToTest (header, []) = Test header Nothing
    blockToTest (header, exp) = Test header (Just $ unlines exp)

lines' = filter (not . liftA2 (&&) (=="") isComment) . lines
  where
    isComment ('-':'-':_) = True
    isComment _ = False

isBlockStart (' ':_) = False
isBlockStart ('=':_) = False -- HACK for :kind responses
isBlockStart _ = True

blocks :: [String] -> [(String, [String])]
blocks (x:xs) | isBlockStart x = let (block, xs') = parseBlock xs in (x, block):blocks xs'
              | otherwise = blocks xs -- FIXME - ignores non-blocks-starts at start of input
  where
    parseBlock (x:xs)
        | isBlockStart x = ([], x:xs)
        | otherwise = let (block, xs') = parseBlock xs in (x:block, xs')
    parseBlock [] = ([], [])

blocks [] = []

testsWithExpectations = mapMaybe getTestWithExpectation
  where
    getTestWithExpectation (Test command expectation) = (command,) <$> expectation

processExpectations :: [(String, Expectation)] -> [(String, [String])] -> [Err]
processExpectations (exp:exps) (blk:blks) = match exp blk ++ processExpectations exps blks
processExpectations [] [] = []
processExpectations ((t, _):_) [] = [t ++ ": no result"]
processExpectations [] ((o, _):_) = ["Extra output: " ++ o]

match :: (String, Expectation) -> (String, [String]) -> [Err]
match (test, expectation) (header, rest)
    | matches expectation rest = []
    | otherwise = [test ++ ": expected:\n" ++ expectation ++ "\n but got:\n" ++ unlines rest]

matches :: Expectation -> [String] -> Bool
matches exp result = words exp == (result >>= words)

runTests :: [Test] -> IO [Err]
runTests tests = do
    let input = unlines $ map testCommand tests
        exps = testsWithExpectations tests

    output <- runGHCi input
    return $ processExpectations exps (blocks . lines' $ output)

runGHCi :: String -> IO String
runGHCi = readProcess "ghci" ["-v0", "-ignore-dot-ghci"]

printErrors :: [Err] -> IO ()
printErrors [] = return ()
printErrors errs = mapM_ putStrLn errs >> exitWith (ExitFailure 1)

runFile :: FilePath -> IO [Err]
runFile file = readTests <$> readFile file >>= runTests

main = getArgs >>= mapM runFile >>= printErrors . concat
