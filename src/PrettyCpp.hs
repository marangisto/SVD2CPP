{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PrettyCpp (preamble, peripheralDecl, parseC) where

import Types as T
import Data.List (isSuffixOf, sortOn)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import System.IO

type Pad = (Int, Int)   -- ident, size

preamble :: String
preamble = unlines
    [ "template<int N>"
    , "class reserved_t"
    , "{"
    , "private:"
    , "    uint32_t m_pad[N];"
    , "};"
    ]

peripheralDecl :: (String -> Maybe Peripheral) -> Peripheral -> String
peripheralDecl findPeripheral p = peripheralStruct findPeripheral p

peripheralStruct :: (String -> Maybe Peripheral) -> Peripheral -> String
peripheralStruct findPeripheral Peripheral{..} = unlines $
    [ "////"
    , "//"
    , "//    " <> unwords (words $ maybe peripheralDescription T.peripheralDescription derived)
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
          ([], rs) = partitionEithers $ maybe peripheralRegisters T.peripheralRegisters derived
          derived = findPeripheral =<< peripheralDerivedFrom

registerStructField :: Register -> String
registerStructField Register{..} = mconcat
    [ "volatile uint32_t    "
    , registerName
    , ";"
    , replicate (14 - length registerName) ' '
    , "// "
    , offset registerAddressOffset
    , unwords (words registerDescription)
    ]

reservedStructField :: Pad -> String
reservedStructField (ident, size) = mconcat
    [ "reserved_t<"
    , str
    , ">"
    , replicate (9 - length str) ' '
    , "_"
    , show ident
    , ";"
    ]
    where str = show (size `div` 4)

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
