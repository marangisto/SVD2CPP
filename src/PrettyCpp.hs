{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PrettyCpp (peripheralDecl, parseC) where

import Types
import Data.List (isSuffixOf, sortOn)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import System.IO

type Pad = (Int, Int)   -- ident, size

peripheralDecl :: (String -> Maybe Peripheral) -> Peripheral -> String
peripheralDecl findPeripheral p = peripheralStruct findPeripheral p

peripheralStruct :: (String -> Maybe Peripheral) -> Peripheral -> String
peripheralStruct findPeripheral Peripheral{..} = unlines $
    [ "////"
    , "//"
    , "//    " <> unwords (words peripheralDescription)
    , "//"
    , "////"
    , ""
    , "struct " <> peripheralName <> "_t"
    , "{"
    ] ++
    map ("    "<>) xs ++
    [ "};"
    , ""
    , mconcat
        [ peripheralName
        , "_t *"
        , peripheralName
        , " = reinterpret_cast<"
        , peripheralName
        , "_t*>("
        , hex peripheralBaseAddress
        , ");"
        ]
    ]
    where xs = map (either reservedStructField registerStructField) $ padRegisters $ removeMe rs
          ([], rs) = partitionEithers $ maybe peripheralRegisters derive peripheralDerivedFrom
          derive from | Just Peripheral{..} <- findPeripheral from = peripheralRegisters
                      | otherwise = error $ "failed to derive peripheral from " ++ from

registerStructField :: Register -> String
registerStructField Register{..} = mconcat
    [ "volatile uint32_t    "
    , registerName
    , ";"
    , pad
    , "// "
    , offset registerAddressOffset
    , unwords (words registerDescription)
    ]
    where pad = replicate (14 - length registerName) ' '

reservedStructField :: Pad -> String
reservedStructField (ident, size) = mconcat $
    [ "uint32_t             "
    , "pad"
    , show ident
    ] ++
    [ "[" <> show (size `div` 4) <> "]"
    | size > 4
    ] ++
    [ ";"
    ]

padRegisters :: [Register] -> [Either Pad Register]
padRegisters = f 0 . sortOn registerAddressOffset
    where f :: Int -> [Register] -> [Either Pad Register]
          f _ (r:[]) = Right r : []
          f i (r0:rs)
              | (r1:_) <- rs
              , registerAddressOffset r1 > registerAddressOffset r0 + 4
              = Right r0 : Left (i, registerAddressOffset r1 - registerAddressOffset r0 - 4) : f (i+1) rs
              | otherwise = Right r0 : f i rs

removeMe = map (\r@Register{..} -> r {registerName = filter (/='%') registerName})

rw :: Maybe AccessType -> String
rw (Just AccessType_Read'only) = "RO"
rw (Just AccessType_Write'only) = "WO"
rw (Just AccessType_Read'write) = "RW"
rw (Just AccessType_WriteOnce) = "WO"
rw (Just AccessType_Read'writeOnce) = "RW"
rw _ = "RW" -- we just don't know more precisely

initCaps :: String -> String
initCaps (x:xs) = toUpper x : map toLower xs

lowerCase :: String -> String
lowerCase = map toLower

hex :: Int -> String
hex x = "0x" ++ showHex x ""

offset :: Int -> String
offset x = "[" ++ show x ++ "]: "

parseC :: FilePath -> IO ()
parseC fn = do
    return ()
{-
    s <- readInputStream fn
    print $ C.parseC s nopos -- (initPos fn)
-}
