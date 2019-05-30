{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PrettyCpp (preamble, postamble, peripheralDecl, parseC) where

import Types as T
import Data.List (isSuffixOf, sortOn)
import Data.List.Extra (stripSuffix, groupSortOn, nubOn)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import Data.Maybe
import Data.Bits
import System.IO

type Pad = (Int, Int)   -- ident, size

preamble :: Device' -> String
preamble Device'{..} = unlines
    [ "#pragma once"
    , ""
    , "#include <stdint.h>"
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
    , ""
    , "template<int N> class reserved_t { private: uint32_t m_pad[N]; };"
    , ""
    , "template<uint8_t POS, uint32_t MASK>"
    , "struct bit_field_t"
    , "{"
    , "    template <uint32_t X>"
    , "    static constexpr uint32_t value()"
    , "    {"
    , "        static_assert((X & ~MASK) == 0, \"field value too large\");"
    , "        return X << POS;"
    , "    }"
    , "};"
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
    , "struct " <> lowerCase peripheralName <> "_t"
    , "{"
    ] ++
    map ("    "<>) xs ++
    concatMap (registerConstants peripheralName) (fixupRegisters rs) ++
    [ "};"
    , ""
    , mconcat
        [ "static "
        , lowerCase peripheralName
        , "_t& "
        , peripheralName
        , " = *reinterpret_cast<"
        , lowerCase peripheralName
        , "_t*>("
        , hex peripheralBaseAddress
        , ");"
        ]
    , ""
    ]
    where xs = map (either reservedStructField registerStructField) $ padRegisters $ fixupRegisters $ removeMe rs
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

registerConstants :: String -> Register -> [String]
registerConstants peripheralName Register{..} =
    "" : map (registerConstant registerName) registerFields ++
    [ "    static const uint32_t " <> registerName <> "_RESET_VALUE = " <> hex x <> ";"
    | Just x <- [ registerResetValue ]
    ]

registerConstant :: String -> Field -> String
registerConstant registerName Field{..}
    | Just 32 <- width = []                 -- elide trivialities
    | Just w <- width, w > 1 = mconcat
        [ "    "
        , "template<uint32_t X>\n"
        , "    "
        , "static constexpr uint32_t "
        , constName
        , " ="
        , replicate (20 - length fieldName) ' '
        , docs
        , "\n"
        , "    "
        , "    bit_field_t<"
        , show offset
        , ", "
        , hex $ shift 0xffffffff (w - 32)
        , ">::value<X>();"
        ]
    | otherwise = mconcat
        [ "    "
        , "static constexpr uint32_t "
        , constName
        , " = " 
        , bit_str
        , ";"
        , replicate (18 - length fieldName - length bit_str) ' '
        , docs
        ]
    where OffsetWidth (offset, width) = fieldPosition
          bit_str = hex $ shift 1 offset
          wfun Nothing = ""
          wfun (Just 1) = ""
          wfun (Just w) = " (" <> show w <> " bits)"
          constName = registerName <> "_" <> fieldName
          docs = mconcat
            [ "// "
            , unwords (words fieldDescription)
            , wfun width
            , maybe "" ((", "<>) . rw) fieldAccess
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

-- | On STMF32 we have overlapping definitions of some timer registers
fixupRegisters :: [Register] -> [Register]
fixupRegisters = map f . groupSortOn registerAddressOffset
    where f [x] = x
          f [x, y] = g x { registerFields = nubOn fieldName $ sortOn fieldName $ registerFields x ++ registerFields y }
          g r@Register{..}
              | Just s <- stripSuffix "_Output" registerName = r { registerName = s }
              | Just s <- stripSuffix "_Input" registerName = r { registerName = s }
              | otherwise = r

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
