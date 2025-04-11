module Main where

import Test.Hspec
import System.Process (callCommand)
import System.Directory (doesFileExist, removeFile)
import System.IO (readFile)

main :: IO ()
main = hspec $ describe "Integration Test for Main.hs" $ do
    mapM_ createTest 
        [ "001", "002", "003", "004", "005", "010", "011", "101", "102", "103", "104", "110", "111", "120", "130", "131", "140", "150", "151", "152", "153",
          "200", "210", "220", "222", "225", "230", "232", "234", "240", "242", "250", "252", "260", "999"
        ]

-- Helper function to create a test for a given test number
createTest :: String -> Spec
createTest testNumber = it ("Processes input-" ++ testNumber ++ ".txt correctly") $ do
    let inputFile = "inputs/input-" ++ testNumber ++ ".txt"
    let expectedFile = "expected/expected-" ++ testNumber ++ ".txt"
    let outputFile = "output-" ++ testNumber ++ ".txt"

    -- Run the Main.hs file using runghc
    callCommand $ "runghc src/Main.hs " ++ inputFile

    -- Check if the output file was created
    outputExists <- doesFileExist outputFile
    outputExists `shouldBe` True

    -- Read and normalize the contents of the output and expected files
    outputContents <- fmap normalizeLineEndings $ readFile outputFile
    expectedContents <- fmap normalizeLineEndings $ readFile expectedFile

    -- Compare the contents
    outputContents `shouldBe` expectedContents

    -- Clean up: Remove the generated output file
    removeFile outputFile

-- Helper function to normalize line endings
normalizeLineEndings :: String -> String
normalizeLineEndings = unlines . lines