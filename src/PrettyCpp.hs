{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeApplications #-}
module PrettyCpp (peripheralDecl, parseC) where

import Types
import Data.List (isSuffixOf)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as M
import Numeric (showHex)
import System.IO

type Pad = Int

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
registerStructField Register{..} = "uint32_t    " <> registerName <> ";" <> pad <> "// " <> offset registerAddressOffset <> unwords (words registerDescription)
    where pad = replicate (14 - length registerName) ' '

reservedStructField :: Pad -> String
reservedStructField _ = "// pad"

-- [ peripheralStruct findPeripheral p, peripheralFun p ]

--pos = OnlyPos nopos (nopos, 0)

removeMe = map (\r@Register{..} -> r {registerName = filter (/='%') registerName})

{-
peripheralFun :: Peripheral -> Item ()
peripheralFun Peripheral{..} = Fn [] PublicV (mkIdent $ lowerCase peripheralName)
    (FnDecl [] (Just (Ptr Mutable (PathTy Nothing (Path False [PathSegment name Nothing ()] ()) ()) ())) False ())
    Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ())
    (Block [ NoSemi (Cast [] (Lit [] (Int Dec addr Unsuffixed ()) ())
             (Ptr Mutable (PathTy Nothing (Path False [PathSegment name Nothing ()] ()) ()) ()) ()) ()
           ] Normal ()) ()
    where name = mkIdent peripheralName
          addr = fromIntegral peripheralBaseAddress

peripheralStruct :: (String -> Maybe Peripheral) -> Peripheral -> Item ()
peripheralStruct findPeripheral Peripheral{..} = StructItem attributes PublicV (mkIdent peripheralName) variantData generics ()
    where variantData = StructD (map (either reservedStructField registerStructField) $ padRegisters $ removeMe rs) ()
          generics = Generics [] [] whereClause ()
          whereClause = WhereClause [] ()
          attributes = [ Attribute Outer (Path False [PathSegment "repr" Nothing ()] ()) (delimTree Paren $ IdentTok "C") ()
                       , SugaredDoc Outer False (' ' : unwords (words peripheralDescription)) ()
                       ]
          ([], rs) = partitionEithers $ maybe peripheralRegisters derive peripheralDerivedFrom
          derive from | Just Peripheral{..} <- findPeripheral from = peripheralRegisters
                      | otherwise = error $ "failed to derive peripheral from " ++ from

-}

padRegisters :: [Register] -> [Either Pad Register]
padRegisters [] = []
padRegisters rs = M.elems $ m `M.union` u
    where m = M.fromList [ (registerAddressOffset r, Right r) | r <- rs ]
          u = M.fromList [ (x, Left x) | x <- [ 0, 4..maximum $ M.keys m ] ]

{-
registerStructField Register{..} = StructField (Just $ mkIdent $ lowerCase registerName) PublicV fieldType attributes ()
    where fieldType = PathTy Nothing (Path False [PathSegment (mkIdent $ rw registerAccess) (Just (AngleBracketed [] [u32Type] [] ())) ()] ()) ()
          attributes = [ SugaredDoc Outer False (' ' : offset registerAddressOffset ++ unwords (words registerDescription)) () ]

reservedStructField x = StructField (Just $ mkIdent $ "reserved" ++ hex x) InheritedV u32Type attributes ()
--StructField (Just "reserved0x30") InheritedV (Array (PathTy Nothing (Path False [PathSegment "u32" Nothing ()] ()) ()) (Lit [] (Int Dec 8 Unsuffixed ()) ()) ()) [] ()
    where attributes = []

u32Type = PathTy Nothing (Path False [PathSegment "u32" Nothing ()] ()) ()

tokenTree x = Tree (Token (Span NoPosition NoPosition) x)

delimTree d x = Tree (Delimited { S.span = Span NoPosition NoPosition, delim = d, tts = tokenTree x })
-}

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
