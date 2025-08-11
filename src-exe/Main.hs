module Main (main) where

import Control.Monad (filterM, forM_, when)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeExtension, takeFileName, (</>))
import System.IO (writeFile)
import TypeChecker

data CompileMode = Compile | Check | Auto deriving (Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> processPath Compile [] path
    ["--check", path] -> processPath Check [] path
    ["--auto", path] -> processPath Auto [] path
    ("--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> processPath Compile (reverse restExcludes) path
        [] -> putStrLn "Usage: tsc [--check|--auto] [--exclude folder1 folder2 ...] <file/directory>"
    ("--check" : "--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> processPath Check (reverse restExcludes) path
        [] -> putStrLn "Usage: tsc [--check|--auto] [--exclude folder1 folder2 ...] <file/directory>"
    ("--auto" : "--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> processPath Auto (reverse restExcludes) path
        [] -> putStrLn "Usage: tsc [--check|--auto] [--exclude folder1 folder2 ...] <file/directory>"
    _ -> putStrLn "Usage: tsc [--check|--auto] [--exclude folder1 folder2 ...] <file/directory>"

processPath :: CompileMode -> [String] -> String -> IO ()
processPath mode excludes path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  if isFile
    then case mode of
      Compile -> processFile path
      Check -> checkFile path
      Auto -> autoProcessFile path
    else
      if isDir
        then case mode of
          Compile -> processDirectoryRecursive excludes path
          Check -> checkDirectoryRecursive excludes path
          Auto -> autoProcessDirectoryRecursive excludes path
        else putStrLn $ "Error: " ++ path ++ " is not a valid file or directory"

processFile :: String -> IO ()
processFile file = do
  content <- readFile file
  let jsOutput = strip_types content
  case jsOutput of
    ('P' : 'a' : 'r' : 's' : 'e' : ' ' : 'e' : 'r' : 'r' : 'o' : 'r' : ':' : _) ->
      putStrLn $ file ++ ": " ++ jsOutput
    _ -> do
      let outputFile = replaceExtension file ".js"
      writeFile outputFile jsOutput
      putStrLn $ "Compiled " ++ file ++ " to " ++ outputFile

autoProcessFile :: String -> IO ()
autoProcessFile file = do
  let jsFile = replaceExtension file ".js"
  jsExists <- doesFileExist jsFile
  if jsExists
    then do
      tsTime <- getModificationTime file
      jsTime <- getModificationTime jsFile
      when (tsTime > jsTime) $ do
        putStrLn $ "T*peScript file is newer, recompiling: " ++ file
        processFile file
    else do
      putStrLn $ "JavaScript file doesn't exist, compiling: " ++ file
      processFile file

processDirectoryRecursive :: [String] -> String -> IO ()
processDirectoryRecursive excludes dir = do
  allFiles <- getAllScriptFiles excludes dir
  if null allFiles
    then putStrLn $ "No T*peScript files found in " ++ dir
    else forM_ allFiles processFile

autoProcessDirectoryRecursive :: [String] -> String -> IO ()
autoProcessDirectoryRecursive excludes dir = do
  allFiles <- getAllScriptFiles excludes dir
  if null allFiles
    then putStrLn $ "No T*peScript files found in " ++ dir
    else forM_ allFiles autoProcessFile

checkFile :: String -> IO ()
checkFile file = do
  content <- readFile file
  putStrLn $ file ++ ": " ++ check_types content

checkDirectoryRecursive :: [String] -> String -> IO ()
checkDirectoryRecursive excludes dir = do
  allFiles <- getAllScriptFiles excludes dir
  if null allFiles
    then putStrLn $ "No T*peScript files found in " ++ dir
    else forM_ allFiles checkFile

getAllScriptFiles :: [String] -> String -> IO [String]
getAllScriptFiles excludes dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  files <- filterM isScriptFile fullPaths
  subdirs <- filterM doesDirectoryExist fullPaths
  let filteredSubdirs = filter (not . isExcluded excludes) subdirs
  subFiles <- mapM (getAllScriptFiles excludes) filteredSubdirs
  return $ files ++ concat subFiles

isExcluded :: [String] -> String -> Bool
isExcluded excludes path =
  let dirName = takeFileName path
   in any (\exclude -> exclude == dirName || exclude `isPrefixOf` dirName) excludes
        || any
          ( \exclude ->
              "node_modules" `isSuffixOf` path
                || ".git" `isSuffixOf` path
                || "dist" `isSuffixOf` path
                || "build" `isSuffixOf` path
          )
          [path]

isScriptFile :: String -> IO Bool
isScriptFile path = do
  isFile <- doesFileExist path
  return $ isFile && takeExtension path `elem` [".ts", ".tsx"]
