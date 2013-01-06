module Main( main ) where

import System.Directory
import System.Environment
import System.FilePath.Posix

main = do
  file     <- fmap offWithHisHead getArgs >>= confirmExists
  newFile  <- (return $ transformPath file) >>= confirmDoesntExist
  isFolder <- doesDirectoryExist file
  let rename = if isFolder then renameDirectory else renameFile
  rename file newFile

offWithHisHead (x : []) = x
offWithHisHead _        = error "usage: dodl file"

delimiterChar = '/'

-- there is some applicative way to do this.
doesFileOrFolderExist f = do
  fileExist   <- doesFileExist f
  folderExist <- doesDirectoryExist f
  return (fileExist || folderExist)

newFileName x = do
             fileExists <- doesFileOrFolderExist x
	     let newFilePath = transformPath x
	     newFileExists <- doesFileOrFolderExist newFilePath
	     return newFilePath

transformPath f = gluePath $ stub ++ [newF]
        where splited = Main.splitPath f
	      stub = init splited
	      newF = transformFileName $ last splited

confirmDoesntExist f = do
        exist <- doesFileOrFolderExist f
	if exist then (error $ f ++ " already exists") else return f

confirmExists :: String -> IO( String )
confirmExists f = do
        exist <- doesFileOrFolderExist f
	if exist then (return f) else error $ f ++ " doesn't exist"

transformFileName ('.':f) = f
transformFileName f = '.' : f

gluePath :: [String] -> String
gluePath (x:[]) = x
gluePath (x:xs) = x ++ [delimiterChar] ++ gluePath xs

splitPath (x:xs) | (isDelimiter x) = "" : splitPath' xs
splitPath xs = splitPath' xs

splitPath' = wordsWhen isDelimiter
isDelimiter = (== delimiterChar)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                           where (w, s'') = break p s'



