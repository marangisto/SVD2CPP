{-# LANGUAGE RecordWildCards, TupleSections #-}
module Traits (peripheralTraitsDecl) where

import Types as T
import Data.List (find, isPrefixOf)
import Data.List.Extra (firstJust)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import Data.Maybe
import Control.Monad

peripheralTraitsDecl :: [Peripheral] -> [String]
peripheralTraitsDecl ps 
    | Just rcc <- findPeripheral "RCC"
    = let ([], rs) = partitionEithers $ peripheralRegisters rcc
          regs = filter (pred . registerName) rs
       in [ "template<typename PERIPHERAL> struct peripheral_traits {};"
          , ""
          ] ++ concat (mapMaybe (fmap prettyPeripheralTraits . peripheralMethods regs) ps)
    | otherwise = []
    where findPeripheral s = find ((==s) . peripheralName) ps
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
    $ maybe [] (uncurry f) (firstJust (findRegisterField2 "EN" "" $ h peripheralName) regs)
   ++ maybe [] (uncurry g) (firstJust (findRegisterField "RST" $ h peripheralName) regs)
    where g registerName fieldName = [ prettyPeripheralMethod "reset" True registerName fieldName ]
          f registerName fieldName = [ prettyPeripheralMethod "enable" True registerName fieldName
                                     , prettyPeripheralMethod "disable" False registerName fieldName
                                     ]
          h "ADC12_Common" = "ADC12"
          h x = x


findRegisterField2 :: String -> String -> String -> Register -> Maybe (String, String)
findRegisterField2 s1 s2 p r = findRegisterField s1 p r `mplus` findRegisterField s2 p r

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

