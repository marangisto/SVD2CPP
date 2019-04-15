{-# LANGUAGE RecordWildCards #-}
module ParseSVD (parseSVD , module Types) where

import Types
import CMSIS_SVD_1_3_3
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Schema.PrimitiveTypes
import Text.XML.HaXml.OneOfN
import Text.ParserCombinators.Poly.Plain
import Data.List (isPrefixOf, intercalate)
import Control.Monad.Extra
import Control.Monad
import Data.Maybe

parseSVD :: FilePath -> IO Device'
parseSVD fn = parseDevice fn <$> readFile fn

parseDevice :: String -> String -> Device'
parseDevice fn xml = seq (errorIfNotNull xs) Device'{..}
    where (Document _ _ root _) = xmlParse fn xml
          Device{..} = either error id $ fst $ runParser elementDevice [CElem root noPos]
          deviceSchemaVersion = decimalToDouble device_schemaVersion
          deviceName = simpleTypeText device_name
          deviceVendor = simpleTypeText <$> device_vendor
          deviceVendorID = simpleTypeText <$> device_vendorID
          deviceSeries = simpleTypeText <$> device_series
          deviceVersion = simpleTypeText device_version
          deviceDescription = stringTypeToString device_description
          deviceLicenseText = stringTypeToString <$> device_licenseText
          deviceHeaderSystemFilename = identifierToString <$> device_headerSystemFilename
          deviceCPU = cpu <$> device_cpu
          deviceAddressUnitBits = scaledNonNegativeIntegerToInt device_addressUnitBits
          deviceWidth = scaledNonNegativeIntegerToInt device_width
          deviceSize = scaledNonNegativeIntegerToInt <$> device_size
          deviceAccess = device_access
          deviceResetValue = scaledNonNegativeIntegerToInt <$> device_resetValue
          deviceResetMask = scaledNonNegativeIntegerToInt <$> device_resetMask
          devicePeripherals = map peripheral $ unPeripherals device_peripherals
          deviceVendorExtensions = vendorExtensions <$> device_vendorExtensions
          xs = [ unhandled "device_headerDefinitionsPrefix" <$> device_headerDefinitionsPrefix 
               , unhandled "device_protection" <$> device_protection 
               ]

peripheral :: PeripheralType -> Peripheral
peripheral PeripheralType{..} = seq (errorIfNotNull xs) Peripheral{..}
    where peripheralName = dimableIdentifierToString peripheralType_name
          peripheralDerivedFrom = dimableIdentifierToString <$> peripheralType_derivedFrom
          peripheralDescription = maybe "" stringTypeToString peripheralType_description
          peripheralVersion = stringTypeToString <$> peripheralType_version
          peripheralGroupName = nameToString <$> peripheralType_groupName
          peripheralPrependToName = identifierToString <$> peripheralType_prependToName
          peripheralBaseAddress = scaledNonNegativeIntegerToInt peripheralType_baseAddress
          peripheralSize = scaledNonNegativeIntegerToInt <$> peripheralType_size
          peripheralAddressBlock = map addressBlock peripheralType_addressBlock
          peripheralInterrupt = map interrupt peripheralType_interrupt
--          Just (RegistersType rs) = peripheralType_registers
          rs = maybe [] registersType_choice0 peripheralType_registers
          peripheralRegisters = map clusterRegister rs
          xs = [ unhandled "peripheralType_dim" <$> peripheralType_dim
               , unhandled "peripheralType_dimIncrement" <$> peripheralType_dimIncrement
               , unhandled "peripheralType_dimIndex" <$> peripheralType_dimIndex
               , unhandled "peripheralType_dimName" <$> peripheralType_dimName
               , unhandled "peripheralType_dimArrayIndex" <$> peripheralType_dimArrayIndex
               , unhandled "peripheralType_alternatePeripheral" <$> peripheralType_alternatePeripheral
               , unhandled "peripheralType_appendToName" <$> peripheralType_appendToName
               , unhandled "peripheralType_headerStructName" <$> peripheralType_headerStructName
               , unhandled "peripheralType_disableCondition" <$> peripheralType_disableCondition
               , unhandled "peripheralType_access" <$> peripheralType_access
               , unhandled "peripheralType_protection" <$> peripheralType_protection
               , unhandled "peripheralType_resetValue" <$> peripheralType_resetValue
               , unhandled "peripheralType_resetMask" <$> peripheralType_resetMask
               ]

clusterRegister :: OneOf2 ClusterType RegisterType -> Either Cluster Register
clusterRegister (OneOf2 _) = error "ClusterType not implemented"
clusterRegister (TwoOf2 r) = Right $ register r

register :: RegisterType -> Register
register RegisterType{..} =
    let registerName = dimableIdentifierToString registerType_name
        registerDisplayName = stringTypeToString <$> registerType_displayName
        registerDescription = maybe "" stringTypeToString registerType_description
        registerAlternative = alternative registerType_choice8
        registerAddressOffset = scaledNonNegativeIntegerToInt registerType_addressOffset
        registerSize = scaledNonNegativeIntegerToInt <$> registerType_size
        registerAccess = registerType_access
        registerResetValue = scaledNonNegativeIntegerToInt <$> registerType_resetValue
        registerResetMask = scaledNonNegativeIntegerToInt <$> registerType_resetMask
        registerModifiedWriteValues = registerType_modifiedWriteValues
        registerDimension = dimension registerType_dim registerType_dimIncrement registerType_dimIndex
        Just (FieldsType fs) = registerType_fields -- :: Maybe FieldsType
        registerFields = map fld fs
    in seq (errorIfNotNull xs) Register{..}
    where xs = [ unhandled "registerType_derivedFrom" <$> registerType_derivedFrom
               , unhandled "registerType_dimName" <$> registerType_dimName
               , unhandled "registerType_dimArrayIndex" <$> registerType_dimArrayIndex
               , unhandled "registerType_protection" <$> registerType_protection
               , unhandled "registerType_dataType" <$> registerType_dataType
               , unhandled "registerType_writeConstraint" <$> registerType_writeConstraint
               , unhandled "registerType_readAction" <$> registerType_readAction
               ]

fld :: FieldType -> Field
fld FieldType{..} =
    let fieldName = dimableIdentifierToString fieldType_name
        fieldDescription = maybe "" stringTypeToString fieldType_description
        fieldPosition =  position fieldType_choice7
        fieldAccess = fieldType_access
    in seq (errorIfNotNull xs) Field{..}
    where xs = [ unhandled "fieldType_derivedFrom" <$> fieldType_derivedFrom
               , unhandled "fieldType_dim" <$> fieldType_dim
               , unhandled "fieldType_dimIncrement" <$> fieldType_dimIncrement
               , unhandled "fieldType_dimIndex" <$> fieldType_dimIndex
               , unhandled "fieldType_dimName" <$> fieldType_dimName
               , unhandled "fieldType_dimArrayIndex" <$> fieldType_dimArrayIndex
               , unhandled "fieldType_modifiedWriteValues" <$> fieldType_modifiedWriteValues
               , unhandled "fieldType_writeConstraint" <$> fieldType_writeConstraint
               , unhandled "fieldType_readAction" <$> fieldType_readAction
               , unhandled "fieldType_enumeratedValues" <$> listToMaybe fieldType_enumeratedValues
               ]

cpu :: CpuType -> CPU
cpu = undefined

position :: OneOf3 (ScaledNonNegativeInteger,ScaledNonNegativeInteger)
                  (ScaledNonNegativeInteger,(Maybe (ScaledNonNegativeInteger)))
                  BitRangeType
         -> Position
position (OneOf3 (x, y)) = LsbMsb (scaledNonNegativeIntegerToInt x, scaledNonNegativeIntegerToInt y)
position (TwoOf3 (x, y)) = OffsetWidth (scaledNonNegativeIntegerToInt x, scaledNonNegativeIntegerToInt <$> y)
position (ThreeOf3 t) = BitRange $ read $ show t

alternative :: Maybe (OneOf2 (Maybe IdentifierType) (Maybe DimableIdentifierType)) -> Maybe (Either String String)
alternative (Just (OneOf2 (Just (IdentifierType x)))) = Just $ Left $ simpleTypeText x
alternative (Just (TwoOf2 (Just (DimableIdentifierType x)))) = Just $ Right $ simpleTypeText x
alternative _ = Nothing

addressBlock :: AddressBlockType -> AddressBlock
addressBlock AddressBlockType{..} =
    let addressBlockOffset = scaledNonNegativeIntegerToInt addressBlockType_offset
        addressBlockSize = scaledNonNegativeIntegerToInt addressBlockType_size
        addressBlockUsage = simpleTypeText addressBlockType_usage
        addressBlockProtection = simpleTypeText <$> addressBlockType_protection
    in AddressBlock{..}

interrupt :: InterruptType -> Interrupt
interrupt InterruptType{..} =
    let interruptName = stringTypeToString interruptType_name
        interruptDescription = maybe "" simpleTypeText interruptType_description
        interruptValue = fromInteger interruptType_value
    in Interrupt{..}

dimension :: Maybe ScaledNonNegativeInteger -> Maybe ScaledNonNegativeInteger -> Maybe DimIndexType -> Maybe Dimension
dimension (Just dim) (Just inc) (Just (DimIndexType idx)) = Just $ Dimension
    ( scaledNonNegativeIntegerToInt dim
    , scaledNonNegativeIntegerToInt inc
    , simpleTypeText idx
    )
dimension Nothing Nothing Nothing = Nothing
dimension _ _ _ = error "unexpexted mix of dimension values"

decimalToDouble (Decimal x) = x

scaledNonNegativeIntegerToInt = read . simpleTypeText . unScaledNonNegativeInteger

identifierToString = simpleTypeText . unIdentifierType

dimableIdentifierToString = simpleTypeText . unDimableIdentifierType

stringTypeToString = simpleTypeText . unStringType

nameToString (Name s) = s

unhandled :: Show a => String -> a -> String
unhandled s x = "unhandled " ++ s ++ ": " ++ show x

errorIfNotNull xs = let ys = catMaybes xs in if null ys then () else error $ intercalate "; " ys

peripheralPrefix :: String -> PeripheralType -> Bool
peripheralPrefix p = (p `isPrefixOf`) . simpleTypeText . unDimableIdentifierType . peripheralType_name

vendorExtensions _ = ()
