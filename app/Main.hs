{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import ParseSVD
import PrettyCpp
import System.Console.CmdArgs
import Text.HTML.TagSoup
import Control.Monad
import Data.List (find)
import Data.Maybe
import System.IO

data Options = Options
    { schema_version    :: Bool
    , interrupt         :: Bool
    , files             :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { schema_version = def &= help "Show schema version"
    , interrupt = def &= help "Generate interrupt vector table"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Generate device descriptions from SVD files" &=
    summary "SVD2CPP v0.0.0, (c) Bengt Marten Agren 2018-2019" &=
    details [ "SVD2CPP generated device header files for ARM-based"
            , "MCUs based on vendor SVD files (see CMSIS)."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs options
    mapM_ (process opts) files

process :: Options -> FilePath -> IO ()
process Options{schema_version=True,..} fn = putStrLn . ((fn++": ")++) =<< getSchemaVersion fn
process Options{interrupt=True,..} fn = do
        dev <- parseSVD fn
        let n = cpuDeviceNumInterrupts =<< deviceCPU dev
        putStrLn $ unlines $ interruptVectorDecl n $ concatMap peripheralInterrupt $ devicePeripherals dev
process Options{..} fn = do
        dev <- parseSVD fn
        let ps = devicePeripherals dev
        putStrLn $ unlines $ preamble dev : map (peripheralDecl (findPeripheral ps)) ps ++ [ postamble ]

findPeripheral :: [Peripheral] -> String -> Maybe Peripheral
findPeripheral ps s = find ((==s) . peripheralName) ps

getSchemaVersion :: FilePath -> IO String
getSchemaVersion fn = f . parseTags <$> readFile fn
    where f = fromAttrib "schemaVersion" . head . dropWhile (~/= ("<device>" :: String))

