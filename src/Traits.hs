{-# LANGUAGE RecordWildCards, TupleSections #-}
module Traits (peripheralTraitsDecl) where

import Types as T
import Data.List (find, isPrefixOf)
import Data.List.Extra (firstJust)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import Data.Maybe

peripheralTraitsDecl :: [Peripheral] -> [String]
peripheralTraitsDecl ps =
    [ "template<typename PERIPHERAL> struct peripheral_traits {};"
    , ""
    ] ++ concat (mapMaybe (fmap prettyPeripheralTraits . peripheralMethods regs) ps)
    where findPeripheral s = find ((==s) . peripheralName) ps
          rcc = fromMaybe (error "failed to locate RCC") $ findPeripheral "RCC"
          ([], rs) = partitionEithers $ peripheralRegisters rcc
          regs = filter (pred . registerName) rs
          pred s = any (`isPrefixOf` s) [ "AHB", "APB", "IOP" ]

prettyPeripheralTraits :: (String, [String]) -> [String]
prettyPeripheralTraits (peripheralName, traits) =
    [ "template<> struct peripheral_traits<" <> lowerCase peripheralName <> "_t>"
    , "{"
    ] ++
    map ("    " <>) traits ++
    [ "};"
    , ""
    ]

peripheralMethods :: [Register] -> Peripheral -> Maybe (String, [String])
peripheralMethods regs Peripheral{..} = (\xs -> if null xs then Nothing else Just (peripheralName, xs))
    $ maybe [] (uncurry f) (firstJust (findRegisterField "EN" peripheralName) regs)
   ++ maybe [] (uncurry g) (firstJust (findRegisterField "RST" peripheralName) regs)
    where g registerName fieldName = [ prettyPeripheralMethod "reset" True registerName fieldName ]
          f registerName fieldName = [ prettyPeripheralMethod "enable" True registerName fieldName
                                     , prettyPeripheralMethod "disable" False registerName fieldName
                                     ]

findRegisterField :: String -> String -> Register -> Maybe (String, String)
findRegisterField suffix peripheralName Register{..} = (registerName,) . fieldName <$> find pred registerFields
    where pred Field{..} 
              | "GPIO" `isPrefixOf` peripheralName
              , "IOP" `isPrefixOf` fieldName
              = "IOP" <> [ last peripheralName ] <> suffix == fieldName
              | otherwise = peripheralName <> suffix == fieldName

prettyPeripheralMethod :: String -> Bool -> String -> String -> String
prettyPeripheralMethod methodName setbit registerName fieldName = mconcat
    [ "static void "
    , methodName
    , "() { RCC."
    , registerName
    , if setbit then " |= " else " &= ~"
    , "rcc_t::"
    , registerName
    , "_" 
    , fieldName
    , "; }"
    ]

lowerCase :: String -> String
lowerCase = map toLower

