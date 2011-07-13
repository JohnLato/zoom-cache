{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Monad.Trans (liftIO)
import Data.Default
import UI.Command

import Zoom.Read
import Zoom.Write

------------------------------------------------------------

zoomGen :: Command ()
zoomGen = defCmd {
          cmdName = "gen"
        , cmdHandler = zoomGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom data"
        , cmdExamples = [("Yo", "")]
        }

zoomGenHandler :: App () ()
zoomGenHandler = liftIO . zoomWriteFile =<< appArgs

zoomWriteFile :: [FilePath] -> IO ()
zoomWriteFile []       = return ()
zoomWriteFile (path:_) = do
    zoomWithFileW path $ do
        liftIO $ putStrLn path
        -- d <- liftIO zoomGenInt
        d <- liftIO zoomGenDouble
        liftIO $ mapM_ (putStrLn . show) d
        mapM_ zoomPutDouble d

-- zoomGenInt :: IO [Int]
-- zoomGenInt = return [3, 3, 4, 3, 3, 6, 6, 7, 4, 9]

zoomGenDouble :: IO [Double]
zoomGenDouble = return [3.5, 3.5, 4.2, 3.7, 3.6, 6.3, 6.7, 7.7, 4.3, 9.3]

------------------------------------------------------------

zoomDump :: Command ()
zoomDump = defCmd {
          cmdName = "dump"
        , cmdHandler = zoomDumpHandler
        , cmdCategory = "Reading"
        , cmdShortDesc = "Read zoom data"
        , cmdExamples = [("Yo", "")]
        }

zoomDumpHandler :: App () ()
zoomDumpHandler = liftIO . zoomReadFile =<< appArgs

------------------------------------------------------------
-- The Application
--

zoom :: Application () ()
zoom = def {
          appName = "zoom"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Trivial zoom inspection tools"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "Zoom"
        , appCmds = [zoomGen, zoomDump]
	}

longDesc :: String
longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain zoom
