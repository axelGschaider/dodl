module Main( main ) where

import System.Directory
import System.Environment
import System.FilePath.Posix

main = do
  file <- fmap offWithHisHead getArgs
  fileExists <- doesFileOrFolderExist file
  print fileExists

offWithHisHead (x : []) = x
offWithHisHead _        = error "usage: dodl file"

doesFileOrFolderExist f = do
  fileExist   <- doesFileExist f
  folderExist <- doesDirectoryExist f
  return (fileExist || folderExist)

