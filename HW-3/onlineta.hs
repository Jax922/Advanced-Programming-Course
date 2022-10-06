#!/usr/bin/env stack
{- stack script
   --resolver lts-19.20
   --package http-client-tls
   --package http-client
   --package zip-archive
   --package dlist
   --package time
   --package filepath
   --package bytestring
   --package directory
   --package process
   --package time
-}

-- Check your submission with OnlineTA <https://find.incorrectness.dk/grade/>.
--
-- This script will zip your `code` directory and try to submit the
-- generated code.zip file to OnlineTA.  Warning: If you already have
-- a code.zip file, this can overwrite files in it.
--
-- Usage: ./onlineta.hs <assignment number> <path-to-code>
--
-- Example use:
--
-- $ ./onlineta.hs ass1 ~/ap/assignment1/code
--
-- This will test the files in code for assignment 1 (ass1).
--
-- This works thanks to the "shebang" at the top of this file.
--
-- Otherwise, you will need to compile this Haskell file to an executable first:
--
-- $ stack ghc -- -Wall onlineta.hs
-- $ ./onlineta ass1 ~/ap/assignment1/code

{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BS
import qualified System.Directory as SD
import qualified Data.DList as DList
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import System.Environment ( getArgs )
import System.FilePath
import Data.String ( fromString )
import Data.List ( find )
import Data.Char
import Control.Monad ( forM, forM_, when )
import Control.Monad.IO.Class ( liftIO )

formURL :: String
formURL = "https://find.incorrectness.dk/grade/"

expectedName = "code"
zipName = "code.zip"

showUsage :: IO ()
showUsage =
  putStrLn "Usage: ./onlineta.hs <assignment name> <path-to-code>"

checkDir :: FilePath -> FilePath -> IO ()
checkDir dir dirExpected = do
  when (takeFileName dir /= dirExpected) $
    fail $ "The hand-in code directory must be called \"" ++ dirExpected ++ "\""
  dirExists     <- SD.doesDirectoryExist dir
  if dirExists
  then return ()
  else showUsage >> fail ("The directory " ++ dir ++ " does not exist.")

-- | Recursively list a directory. Do not return paths to empty directories.
listDirRecur :: FilePath -> (FilePath -> Bool) -> IO [(FilePath, FilePath)]
listDirRecur path excluding = DList.toList <$> go ""
  where
    go adir = do
      let cdir = path </> adir
      raw <- SD.listDirectory cdir
      fmap mconcat . forM raw $ \ fp ->
        case fp of
          ""              -> return mempty
          x | excluding x -> return mempty
          x               -> do
            let fullx = cdir </> x
                adir' = adir </> x
            isFile <- SD.doesFileExist      fullx
            isDir  <- SD.doesDirectoryExist fullx
            if isFile
              then return $ DList.singleton (adir', fullx)
              else if isDir
                   then go adir'
                   else return mempty

archive :: FilePath -> FilePath -> IO ()
archive dir zipFile = do
  files <- listDirRecur dir $ \fp -> head fp == '.'
  entries <- forM files $ \ (relative, fullPath) -> do
    contents <- BS.readFile fullPath
    modEpochTime <- (floor . utcTimeToPOSIXSeconds) <$> SD.getModificationTime fullPath
    let inZipPath = expectedName </> relative
    return $ Zip.toEntry inZipPath modEpochTime contents
  let arch = foldr Zip.addEntryToArchive Zip.emptyArchive entries
  BS.writeFile zipFile (Zip.fromArchive arch)

checkSubmission :: FilePath -> String -> IO ()
checkSubmission zipFile url = do
  manager <- newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  request <- parseUrlThrow url
  let files = [partFileSource "handin" zipFile]
  body <- formDataBody files request
  response <- responseBody <$> httpLbs body manager
  BS.putStr response
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [assignment, dir] -> do
      checkDir dir expectedName
      putStrLn "Zipping files"
      archive dir zipName
      putStrLn "Sending code.zip to server (wait for for it...)"
      checkSubmission zipName (formURL ++ assignment)
    _ -> showUsage
