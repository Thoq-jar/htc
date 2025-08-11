module Main (main) where

import Control.Monad (filterM, forM_, when)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeExtension, takeFileName, (</>))
import System.IO (writeFile)
import TypeChecker (checkTypes, stripTypes)

data CompileMode = Compile | Check | Auto deriving (Eq)

colors :: [(String, String)]
colors =
  [ ("reset", "\ESC[0m"),
    ("black", "\ESC[30m"),
    ("red", "\ESC[31m"),
    ("green", "\ESC[32m"),
    ("yellow", "\ESC[33m"),
    ("blue", "\ESC[34m"),
    ("magenta", "\ESC[35m"),
    ("cyan", "\ESC[36m"),
    ("white", "\ESC[37m"),
    ("brightBlack", "\ESC[90m"),
    ("brightRed", "\ESC[91m"),
    ("brightGreen", "\ESC[92m"),
    ("brightYellow", "\ESC[93m"),
    ("brightBlue", "\ESC[94m"),
    ("brightMagenta", "\ESC[95m"),
    ("brightCyan", "\ESC[96m"),
    ("brightWhite", "\ESC[97m")
  ]

colorCode :: String -> String
colorCode name = Data.Maybe.fromMaybe "" (lookup name colors)

asciiArt :: String
asciiArt =
  colorCode "brightMagenta"
    ++ "██╗  ██╗████████╗ ██████╗\n\
       \██║  ██║╚══██╔══╝██╔════╝\n\
       \███████║   ██║   ██║     \n\
       \██╔══██║   ██║   ██║     \n\
       \██║  ██║   ██║   ╚██████╗\n\
       \╚═╝  ╚═╝   ╚═╝    ╚═════╝\n\
       \  Haskell Type Checker   \n"
    ++ colorCode "reset"

main :: IO ()
main = do
  putStrLn asciiArt

  args <- getArgs
  let usage = colorCode "green" ++ "Usage: tsc [--check|--auto] [--exclude folder1 folder2 ...] <file/directory>" ++ colorCode "reset"
      info msg = putStrLn $ colorCode "brightCyan" ++ msg ++ colorCode "reset"
      errorMsg msg = putStrLn $ colorCode "brightRed" ++ msg ++ colorCode "reset"
  case args of
    [path] -> info ("Compiling: " ++ path) >> processPath Compile [] path
    ["--check", path] -> info ("Checking validity of types... [" ++ path ++ "]") >> processPath Check [] path
    ["--auto", path] -> info ("Auto mode: " ++ path) >> processPath Auto [] path
    ("--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> info ("Compiling (excluding: " ++ show restExcludes ++ "): " ++ path) >> processPath Compile (reverse restExcludes) path
        [] -> errorMsg usage
    ("--check" : "--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> info ("Checking validity of types... (excluding: " ++ show restExcludes ++ "): " ++ path) >> processPath Check (reverse restExcludes) path
        [] -> errorMsg usage
    ("--auto" : "--exclude" : excludes) ->
      case reverse excludes of
        (path : restExcludes) -> info ("Auto mode (excluding: " ++ show restExcludes ++ "): " ++ path) >> processPath Auto (reverse restExcludes) path
        [] -> errorMsg usage
    _ -> errorMsg usage

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
  let jsOutput = stripTypes content
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
        putStrLn $ "HackScript file is newer, recompiling: " ++ file
        processFile file
    else do
      putStrLn $ "JS file doesn't exist, compiling: " ++ file
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
  putStrLn $ file ++ ": " ++ checkTypes content

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
        || ( \exclude ->
               "node_modules" `isSuffixOf` path
                 || ".git" `isSuffixOf` path
                 || "dist" `isSuffixOf` path
                 || "build" `isSuffixOf` path
           )
          path

isScriptFile :: String -> IO Bool
isScriptFile path = do
  isFile <- doesFileExist path
  return $ isFile && takeExtension path `elem` [".ts", ".tsx", ".hts", ".htx"]
