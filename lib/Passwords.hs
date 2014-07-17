module Passwords (
    getPasswords
  , passwordPrompt
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Posix (dropExtension, makeRelative)
import System.Environment (getEnv)

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
  dir <- getEnv "PASSWORD_STORE_DIR"
  let password_dir = dir </> ""
  files <- getFiles password_dir
  return $ map ((makeRelative password_dir) . dropExtension) files

data Pass = Pass

instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete  _ c = c
  nextCompletion       _ = getNextCompletion

selectPassword :: String -> X ()
selectPassword ps = spawn $ "pass -c " ++ ps

passwordPrompt :: XPConfig -> X ()
passwordPrompt config = do
  li <- io getPasswords
  mkXPrompt Pass config (mkComplFunFromList li) selectPassword
