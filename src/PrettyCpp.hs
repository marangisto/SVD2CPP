{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PrettyCpp (preamble, postamble, peripheralDecl, parseC) where

import Types as T
import Data.List (isSuffixOf, sortOn)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import Data.Maybe
import System.IO

type Pad = (Int, Int)   -- ident, size

preamble :: Device' -> String
preamble Device'{..} = unlines
    [ "#pragma once"
    , ""
    , "template<int N> class reserved_t { private: uint32_t m_pad[N]; };"
    , ""
    , "static inline uint32_t BV(uint8_t x) { return 1 << x; }"
    , ""
    , "////"
    , "//"
    , "//    " <> deviceName
    , "//"
    , "//       schema-version : " <> show deviceSchemaVersion
    , "//       vendor         : " <> fromMaybe "" deviceVendor
    , "//       series         : " <> fromMaybe "" deviceSeries
    , "//       device-version : " <> deviceVersion
    , "//       address-unit   : " <> show deviceAddressUnitBits <> " bits"
    , "//       device-width   : " <> show deviceWidth
    , "//       device-size    : " <> maybe "" show deviceSize
    , "//"
    , "////"
    , ""
    , "namespace " <> lowerCase deviceName
    , "{"
    ]

postamble :: String
postamble = unlines
    [ "}"
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
    , "namespace " <> lowerCase peripheralName
    , "{"
    , ""
    , "struct " <> lowerCase peripheralName <> "_t"
    , "{"
    ] ++
    map ("    "<>) xs ++
    [ "};"
    , ""
    , mconcat
        [ lowerCase peripheralName
        , "_t& "
        , peripheralName
        , " = *reinterpret_cast<"
        , lowerCase peripheralName
        , "_t*>("
        , hex peripheralBaseAddress
        , ");"
        ]
    , ""
    ] ++
    map (registerConstants peripheralName) rs ++
    [ "}"
    ]
    where xs = map (either reservedStructField registerStructField) $ padRegisters $ removeMe rs
          ([], rs) = partitionEithers $ maybe peripheralRegisters T.peripheralRegisters derived
          derived = findPeripheral =<< peripheralDerivedFrom

registerStructField :: Register -> String
registerStructField Register{..} = mconcat
    [ "volatile uint32_t    "
    , registerName
    , ";"
    , replicate (21 - length registerName) ' '
    , "// "
    , maybe "" ((<> "] ") . ("["<>) . rw) registerAccess
    , unwords (words registerDescription)
    ]

registerConstants :: String -> Register -> String
registerConstants peripheralName Register{..} = unlines $
    [ "namespace " <> registerName <> " // " <> unwords (words registerDescription) <> " fields"
    , "{" 
    ] ++
    map f registerFields ++
    [ "}"
    ]
    where f Field{..} = mconcat
            [ "    "
            , "static const uint8_t "
            , fieldName
            , " = " 
            , offset_str
            , ";"
            , replicate (18 - length fieldName - length offset_str) ' '
            , "// "
            , unwords (words fieldDescription)
            , wfun width
            , maybe "" ((", "<>) . rw) fieldAccess
            ]
            where OffsetWidth (offset, width) = fieldPosition
                  offset_str = show offset
                  wfun Nothing = ""
                  wfun (Just 1) = ""
                  wfun (Just w) = " (" <> show w <> " bits)"

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

rw :: AccessType -> String
rw AccessType_Read'only = "Read-only"
rw AccessType_Write'only = "Write-only"
rw AccessType_Read'write = "Read-write"
rw AccessType_WriteOnce = "Write-once"
rw AccessType_Read'writeOnce = "Read-write-once"

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
