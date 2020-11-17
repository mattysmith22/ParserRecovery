module Main (main) where

import           System.IO

import           System.Environment
import           Text.Groom
import           Triangle.Parser

doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
    handle <- openFile path ReadMode
    readable <- hIsReadable handle
    hClose handle
    return readable

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

splitFile :: FilePath -> (FilePath, FilePath)
splitFile path = (init $ reverse base, reverse ext)
    where
        (ext, base) = span (/= '.') $ reverse path

compileMt :: FilePath -> IO ()
compileMt path = do
    expFile <- readFile path
    case parseProgram expFile of
        Left err -> do
            putStrLn "Error while parsing:"
            putStrLn $ groom err
        Right ast -> do
            putStrLn $ groom ast

handleFile :: FilePath -> IO ()
handleFile path = do
    fileExist <- doesFileExist path
    if fileExist then do
        case takeWhileEnd (/='.') path of
            "mt" -> compileMt path
            x -> do
                putStrLn $ ("Error: Unknown extension \"" ++ x ++ "\".")
                printHelp
    else do
        putStrLn "Error: File does not exist"
        printHelp

printHelp :: IO ()
printHelp = putStrLn "Enter either a triangle language file (.mt) to compile or"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] -> do
            handleFile x
        _ -> printHelp
