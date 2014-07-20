module Passwords (
    getPasswords
  , passwordPrompt
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Posix (dropExtension, makeRelative)
import System.Environment (getEnv, lookupEnv)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)

import XMonad.Core
import XMonad.Prompt

getFiles dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [ ".", "..", ".git" ]) names
  paths <- forM properNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getFiles path
      else return [path]
  return (concat paths)

getPasswords = do
  password_dir <- getPasswordDir
  files <- getFiles password_dir
  return $ map ((makeRelative password_dir) . dropExtension) files

getPasswordDir = do
  envDir <- lookupEnv "PASSWORD_STORE_DIR"
  home <- getEnv "HOME"
  return $ fromMaybe (home </> ".password_store") envDir

data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete  _ c = c
  nextCompletion       _ = getNextCompletion

selectPassword :: [String] -> String -> X ()
selectPassword passwords ps = spawn $ "pass " ++ args
  where
    args | ps `elem` passwords = "show -c " ++ ps
         | otherwise = "generate -c " ++ ps ++ " 24"

passwordPrompt :: XPConfig -> X ()
passwordPrompt config = do
  li <- io getPasswords
  let compl = \s -> filter (\x -> s `isInfixOf` x) li
  mkXPrompt Pass config (return . compl) (selectPassword li)
