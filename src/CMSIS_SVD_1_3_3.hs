{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module CMSIS_SVD_1_3_3
  ( module CMSIS_SVD_1_3_3
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype Range = Range (Int, Int) deriving (Eq, Show)
instance SchemaType Range where
    parseSchemaType s = error "FIXME: Range not implemented"
    schemaTypeToXML s (Range _) = error "toXML not implemented"
newtype SauRegionsConfig = SauRegionsConfig { unSauRegionsConfig :: [()] } deriving (Eq,Show)
instance SchemaType SauRegionsConfig where
    parseSchemaType s = do
        (pos, e) <- posnElement [s]
        undefined   -- FIXME: handle this if we ever come accross it.
    schemaTypeToXML s (SauRegionsConfig _) = error "toXML not implemented"
newtype Peripherals = Peripherals { unPeripherals :: [PeripheralType] } deriving (Eq,Show)
instance SchemaType Peripherals where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return (Peripherals) `apply` many (parseSchemaType "peripheral")
    schemaTypeToXML s (Peripherals xs) = error "toXML not implemented"
newtype VendorExtensions = VendorExtensions () deriving (Eq,Show)
instance SchemaType VendorExtensions where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ return (VendorExtensions ())
    schemaTypeToXML s (VendorExtensions _) = error "toXML not implemented"
 
newtype StringType = StringType { unStringType :: Xsd.XsdString } deriving (Eq,Show)
instance Restricts StringType Xsd.XsdString where
    restricts (StringType x) = x
instance SchemaType StringType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (StringType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType StringType where
    acceptingParser = fmap StringType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (StrLength (Occurs (Just 1) Nothing))
    simpleTypeText (StringType x) = simpleTypeText x
 
newtype DescriptionStringType = DescriptionStringType Xsd.XsdString deriving (Eq,Show)
instance Restricts DescriptionStringType Xsd.XsdString where
    restricts (DescriptionStringType x) = x
instance SchemaType DescriptionStringType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DescriptionStringType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DescriptionStringType where
    acceptingParser = fmap DescriptionStringType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [\p{IsBasicLatin}\p{IsLatin-1Supplement}]*)
    simpleTypeText (DescriptionStringType x) = simpleTypeText x
 
data CpuNameType
    = CpuNameType_CM0
    | CpuNameType_CM0PLUS
    | CpuNameType_CM0'
    | CpuNameType_CM1
    | CpuNameType_SC000
    | CpuNameType_CM23
    | CpuNameType_CM3
    | CpuNameType_CM33
    | CpuNameType_SC300
    | CpuNameType_CM4
    | CpuNameType_CM7
    | CpuNameType_ARMV8MML
    | CpuNameType_ARMV8MBL
    | CpuNameType_CA5
    | CpuNameType_CA7
    | CpuNameType_CA8
    | CpuNameType_CA9
    | CpuNameType_CA15
    | CpuNameType_CA17
    | CpuNameType_CA53
    | CpuNameType_CA57
    | CpuNameType_CA72
    | CpuNameType_Other
    deriving (Eq,Show,Enum)
instance SchemaType CpuNameType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CpuNameType where
    acceptingParser =  do literal "CM0"; return CpuNameType_CM0
                      `onFail` do literal "CM0PLUS"; return CpuNameType_CM0PLUS
                      `onFail` do literal "CM0+"; return CpuNameType_CM0'
                      `onFail` do literal "CM1"; return CpuNameType_CM1
                      `onFail` do literal "SC000"; return CpuNameType_SC000
                      `onFail` do literal "CM23"; return CpuNameType_CM23
                      `onFail` do literal "CM3"; return CpuNameType_CM3
                      `onFail` do literal "CM33"; return CpuNameType_CM33
                      `onFail` do literal "SC300"; return CpuNameType_SC300
                      `onFail` do literal "CM4"; return CpuNameType_CM4
                      `onFail` do literal "CM7"; return CpuNameType_CM7
                      `onFail` do literal "ARMV8MML"; return CpuNameType_ARMV8MML
                      `onFail` do literal "ARMV8MBL"; return CpuNameType_ARMV8MBL
                      `onFail` do literal "CA5"; return CpuNameType_CA5
                      `onFail` do literal "CA7"; return CpuNameType_CA7
                      `onFail` do literal "CA8"; return CpuNameType_CA8
                      `onFail` do literal "CA9"; return CpuNameType_CA9
                      `onFail` do literal "CA15"; return CpuNameType_CA15
                      `onFail` do literal "CA17"; return CpuNameType_CA17
                      `onFail` do literal "CA53"; return CpuNameType_CA53
                      `onFail` do literal "CA57"; return CpuNameType_CA57
                      `onFail` do literal "CA72"; return CpuNameType_CA72
                      `onFail` do literal "other"; return CpuNameType_Other
                      
    simpleTypeText CpuNameType_CM0 = "CM0"
    simpleTypeText CpuNameType_CM0PLUS = "CM0PLUS"
    simpleTypeText CpuNameType_CM0' = "CM0+"
    simpleTypeText CpuNameType_CM1 = "CM1"
    simpleTypeText CpuNameType_SC000 = "SC000"
    simpleTypeText CpuNameType_CM23 = "CM23"
    simpleTypeText CpuNameType_CM3 = "CM3"
    simpleTypeText CpuNameType_CM33 = "CM33"
    simpleTypeText CpuNameType_SC300 = "SC300"
    simpleTypeText CpuNameType_CM4 = "CM4"
    simpleTypeText CpuNameType_CM7 = "CM7"
    simpleTypeText CpuNameType_ARMV8MML = "ARMV8MML"
    simpleTypeText CpuNameType_ARMV8MBL = "ARMV8MBL"
    simpleTypeText CpuNameType_CA5 = "CA5"
    simpleTypeText CpuNameType_CA7 = "CA7"
    simpleTypeText CpuNameType_CA8 = "CA8"
    simpleTypeText CpuNameType_CA9 = "CA9"
    simpleTypeText CpuNameType_CA15 = "CA15"
    simpleTypeText CpuNameType_CA17 = "CA17"
    simpleTypeText CpuNameType_CA53 = "CA53"
    simpleTypeText CpuNameType_CA57 = "CA57"
    simpleTypeText CpuNameType_CA72 = "CA72"
    simpleTypeText CpuNameType_Other = "other"
 
newtype RevisionType = RevisionType Xsd.XsdString deriving (Eq,Show)
instance Restricts RevisionType Xsd.XsdString where
    restricts (RevisionType x) = x
instance SchemaType RevisionType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (RevisionType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RevisionType where
    acceptingParser = fmap RevisionType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern r[0-9]*p[0-9]*)
    simpleTypeText (RevisionType x) = simpleTypeText x
 
data EndianType
    = EndianType_Little
    | EndianType_Big
    | EndianType_Selectable
    | EndianType_Other
    deriving (Eq,Show,Enum)
instance SchemaType EndianType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EndianType where
    acceptingParser =  do literal "little"; return EndianType_Little
                      `onFail` do literal "big"; return EndianType_Big
                      `onFail` do literal "selectable"; return EndianType_Selectable
                      `onFail` do literal "other"; return EndianType_Other
                      
    simpleTypeText EndianType_Little = "little"
    simpleTypeText EndianType_Big = "big"
    simpleTypeText EndianType_Selectable = "selectable"
    simpleTypeText EndianType_Other = "other"
 
data DataTypeType
    = DataTypeType_Uint8_t
    | DataTypeType_Uint16_t
    | DataTypeType_Uint32_t
    | DataTypeType_Uint64_t
    | DataTypeType_Int8_t
    | DataTypeType_Int16_t
    | DataTypeType_Int32_t
    | DataTypeType_Int64_t
    | DataTypeType_Uint8_t_'
    | DataTypeType_Uint16_t_'
    | DataTypeType_Uint32_t_'
    | DataTypeType_Uint64_t_'
    | DataTypeType_Int8_t_'
    | DataTypeType_Int16_t_'
    | DataTypeType_Int32_t_'
    | DataTypeType_Int64_t_'
    deriving (Eq,Show,Enum)
instance SchemaType DataTypeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DataTypeType where
    acceptingParser =  do literal "uint8_t"; return DataTypeType_Uint8_t
                      `onFail` do literal "uint16_t"; return DataTypeType_Uint16_t
                      `onFail` do literal "uint32_t"; return DataTypeType_Uint32_t
                      `onFail` do literal "uint64_t"; return DataTypeType_Uint64_t
                      `onFail` do literal "int8_t"; return DataTypeType_Int8_t
                      `onFail` do literal "int16_t"; return DataTypeType_Int16_t
                      `onFail` do literal "int32_t"; return DataTypeType_Int32_t
                      `onFail` do literal "int64_t"; return DataTypeType_Int64_t
                      `onFail` do literal "uint8_t *"; return DataTypeType_Uint8_t_'
                      `onFail` do literal "uint16_t *"; return DataTypeType_Uint16_t_'
                      `onFail` do literal "uint32_t *"; return DataTypeType_Uint32_t_'
                      `onFail` do literal "uint64_t *"; return DataTypeType_Uint64_t_'
                      `onFail` do literal "int8_t *"; return DataTypeType_Int8_t_'
                      `onFail` do literal "int16_t *"; return DataTypeType_Int16_t_'
                      `onFail` do literal "int32_t *"; return DataTypeType_Int32_t_'
                      `onFail` do literal "int64_t *"; return DataTypeType_Int64_t_'
                      
    simpleTypeText DataTypeType_Uint8_t = "uint8_t"
    simpleTypeText DataTypeType_Uint16_t = "uint16_t"
    simpleTypeText DataTypeType_Uint32_t = "uint32_t"
    simpleTypeText DataTypeType_Uint64_t = "uint64_t"
    simpleTypeText DataTypeType_Int8_t = "int8_t"
    simpleTypeText DataTypeType_Int16_t = "int16_t"
    simpleTypeText DataTypeType_Int32_t = "int32_t"
    simpleTypeText DataTypeType_Int64_t = "int64_t"
    simpleTypeText DataTypeType_Uint8_t_' = "uint8_t *"
    simpleTypeText DataTypeType_Uint16_t_' = "uint16_t *"
    simpleTypeText DataTypeType_Uint32_t_' = "uint32_t *"
    simpleTypeText DataTypeType_Uint64_t_' = "uint64_t *"
    simpleTypeText DataTypeType_Int8_t_' = "int8_t *"
    simpleTypeText DataTypeType_Int16_t_' = "int16_t *"
    simpleTypeText DataTypeType_Int32_t_' = "int32_t *"
    simpleTypeText DataTypeType_Int64_t_' = "int64_t *"
 
newtype NvicPrioBitsType = NvicPrioBitsType Xsd.Integer deriving (Eq,Show)
instance Restricts NvicPrioBitsType Xsd.Integer where
    restricts (NvicPrioBitsType x) = x
instance SchemaType NvicPrioBitsType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (NvicPrioBitsType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NvicPrioBitsType where
    acceptingParser = fmap NvicPrioBitsType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs (Just 2) (Just 8)))
    simpleTypeText (NvicPrioBitsType x) = simpleTypeText x
 
newtype DimableIdentifierType = DimableIdentifierType { unDimableIdentifierType :: Xsd.XsdString } deriving (Eq,Show)
instance Restricts DimableIdentifierType Xsd.XsdString where
    restricts (DimableIdentifierType x) = x
instance SchemaType DimableIdentifierType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DimableIdentifierType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DimableIdentifierType where
    acceptingParser = fmap DimableIdentifierType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern ((%s)|(%s)[_A-Za-z]{1}[_A-Za-z0-9]*)|([_A-Za-z]{1}[_A-Za-z0-9]*(\[%s\])?)|([_A-Za-z]{1}[_A-Za-z0-9]*(%s)?[_A-Za-z0-9]*))
    simpleTypeText (DimableIdentifierType x) = simpleTypeText x
 
newtype IdentifierType = IdentifierType { unIdentifierType :: Xsd.XsdString } deriving (Eq,Show)
instance Restricts IdentifierType Xsd.XsdString where
    restricts (IdentifierType x) = x
instance SchemaType IdentifierType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (IdentifierType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IdentifierType where
    acceptingParser = fmap IdentifierType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [_A-Za-z0-9]*)
    simpleTypeText (IdentifierType x) = simpleTypeText x
 
newtype ProtectionStringType = ProtectionStringType Xsd.XsdString deriving (Eq,Show)
instance Restricts ProtectionStringType Xsd.XsdString where
    restricts (ProtectionStringType x) = x
instance SchemaType ProtectionStringType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ProtectionStringType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ProtectionStringType where
    acceptingParser = fmap ProtectionStringType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [snp])
    simpleTypeText (ProtectionStringType x) = simpleTypeText x
 
newtype SauAccessType = SauAccessType Xsd.XsdString deriving (Eq,Show)
instance Restricts SauAccessType Xsd.XsdString where
    restricts (SauAccessType x) = x
instance SchemaType SauAccessType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (SauAccessType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SauAccessType where
    acceptingParser = fmap SauAccessType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [cn])
    simpleTypeText (SauAccessType x) = simpleTypeText x
 
newtype DimIndexType = DimIndexType Xsd.XsdString deriving (Eq,Show)
instance Restricts DimIndexType Xsd.XsdString where
    restricts (DimIndexType x) = x
instance SchemaType DimIndexType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (DimIndexType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DimIndexType where
    acceptingParser = fmap DimIndexType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [0-9]+\-[0-9]+|[A-Z]-[A-Z]|[_0-9a-zA-Z]+(,\s*[_0-9a-zA-Z]+)+)
    simpleTypeText (DimIndexType x) = simpleTypeText x
 
newtype ScaledNonNegativeInteger = ScaledNonNegativeInteger { unScaledNonNegativeInteger :: Xsd.XsdString } deriving (Eq,Show)
instance Restricts ScaledNonNegativeInteger Xsd.XsdString where
    restricts (ScaledNonNegativeInteger x) = x
instance SchemaType ScaledNonNegativeInteger where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (ScaledNonNegativeInteger x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ScaledNonNegativeInteger where
    acceptingParser = fmap ScaledNonNegativeInteger acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [+]?(0x|0X|#)?[0-9a-fA-F]+[kmgtKMGT]?)
    simpleTypeText (ScaledNonNegativeInteger x) = simpleTypeText x
 
newtype EnumeratedValueDataType = EnumeratedValueDataType Xsd.XsdString deriving (Eq,Show)
instance Restricts EnumeratedValueDataType Xsd.XsdString where
    restricts (EnumeratedValueDataType x) = x
instance SchemaType EnumeratedValueDataType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (EnumeratedValueDataType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EnumeratedValueDataType where
    acceptingParser = fmap EnumeratedValueDataType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [+]?(((0x|0X)[0-9a-fA-F]+)|([0-9]+)|((#|0b)[01xX]+)))
    simpleTypeText (EnumeratedValueDataType x) = simpleTypeText x
 
data AccessType
    = AccessType_Read'only
    | AccessType_Write'only
    | AccessType_Read'write
    | AccessType_WriteOnce
    | AccessType_Read'writeOnce
    deriving (Eq,Show,Enum)
instance SchemaType AccessType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AccessType where
    acceptingParser =  do literal "read-only"; return AccessType_Read'only
                      `onFail` do literal "write-only"; return AccessType_Write'only
                      `onFail` do literal "read-write"; return AccessType_Read'write
                      `onFail` do literal "writeOnce"; return AccessType_WriteOnce
                      `onFail` do literal "read-writeOnce"; return AccessType_Read'writeOnce
                      
    simpleTypeText AccessType_Read'only = "read-only"
    simpleTypeText AccessType_Write'only = "write-only"
    simpleTypeText AccessType_Read'write = "read-write"
    simpleTypeText AccessType_WriteOnce = "writeOnce"
    simpleTypeText AccessType_Read'writeOnce = "read-writeOnce"
 
data ModifiedWriteValuesType
    = ModifiedWriteValuesType_OneToClear
    | ModifiedWriteValuesType_OneToSet
    | ModifiedWriteValuesType_OneToToggle
    | ModifiedWriteValuesType_ZeroToClear
    | ModifiedWriteValuesType_ZeroToSet
    | ModifiedWriteValuesType_ZeroToToggle
    | ModifiedWriteValuesType_Clear
    | ModifiedWriteValuesType_Set
    | ModifiedWriteValuesType_Modify
    deriving (Eq,Show,Enum)
instance SchemaType ModifiedWriteValuesType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ModifiedWriteValuesType where
    acceptingParser =  do literal "oneToClear"; return ModifiedWriteValuesType_OneToClear
                      `onFail` do literal "oneToSet"; return ModifiedWriteValuesType_OneToSet
                      `onFail` do literal "oneToToggle"; return ModifiedWriteValuesType_OneToToggle
                      `onFail` do literal "zeroToClear"; return ModifiedWriteValuesType_ZeroToClear
                      `onFail` do literal "zeroToSet"; return ModifiedWriteValuesType_ZeroToSet
                      `onFail` do literal "zeroToToggle"; return ModifiedWriteValuesType_ZeroToToggle
                      `onFail` do literal "clear"; return ModifiedWriteValuesType_Clear
                      `onFail` do literal "set"; return ModifiedWriteValuesType_Set
                      `onFail` do literal "modify"; return ModifiedWriteValuesType_Modify
                      
    simpleTypeText ModifiedWriteValuesType_OneToClear = "oneToClear"
    simpleTypeText ModifiedWriteValuesType_OneToSet = "oneToSet"
    simpleTypeText ModifiedWriteValuesType_OneToToggle = "oneToToggle"
    simpleTypeText ModifiedWriteValuesType_ZeroToClear = "zeroToClear"
    simpleTypeText ModifiedWriteValuesType_ZeroToSet = "zeroToSet"
    simpleTypeText ModifiedWriteValuesType_ZeroToToggle = "zeroToToggle"
    simpleTypeText ModifiedWriteValuesType_Clear = "clear"
    simpleTypeText ModifiedWriteValuesType_Set = "set"
    simpleTypeText ModifiedWriteValuesType_Modify = "modify"
 
data ReadActionType
    = ReadActionType_Clear
    | ReadActionType_Set
    | ReadActionType_Modify
    | ReadActionType_ModifyExternal
    deriving (Eq,Show,Enum)
instance SchemaType ReadActionType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReadActionType where
    acceptingParser =  do literal "clear"; return ReadActionType_Clear
                      `onFail` do literal "set"; return ReadActionType_Set
                      `onFail` do literal "modify"; return ReadActionType_Modify
                      `onFail` do literal "modifyExternal"; return ReadActionType_ModifyExternal
                      
    simpleTypeText ReadActionType_Clear = "clear"
    simpleTypeText ReadActionType_Set = "set"
    simpleTypeText ReadActionType_Modify = "modify"
    simpleTypeText ReadActionType_ModifyExternal = "modifyExternal"
 
data EnumUsageType
    = EnumUsageType_Read
    | EnumUsageType_Write
    | EnumUsageType_Read'write
    deriving (Eq,Show,Enum)
instance SchemaType EnumUsageType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EnumUsageType where
    acceptingParser =  do literal "read"; return EnumUsageType_Read
                      `onFail` do literal "write"; return EnumUsageType_Write
                      `onFail` do literal "read-write"; return EnumUsageType_Read'write
                      
    simpleTypeText EnumUsageType_Read = "read"
    simpleTypeText EnumUsageType_Write = "write"
    simpleTypeText EnumUsageType_Read'write = "read-write"
 
newtype BitRangeType = BitRangeType Xs.Token deriving (Eq,Show)
instance Restricts BitRangeType Xs.Token where
    restricts (BitRangeType x) = x
instance SchemaType BitRangeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (BitRangeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType BitRangeType where
    acceptingParser = fmap BitRangeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern \[([0-4])?[0-9]:([0-4])?[0-9]\])
    simpleTypeText (BitRangeType x) = simpleTypeText x
 
data WriteConstraintType = WriteConstraintType
        { writeConstraintType_choice0 :: OneOf3 Xsd.Boolean Xsd.Boolean Range
          -- ^ Choice between:
          --   
          --   (1) writeAsRead
          --   
          --   (2) useEnumeratedValues
          --   
          --   (3) range
        }
        deriving (Eq,Show)
instance SchemaType WriteConstraintType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return WriteConstraintType
            `apply` oneOf' [ ("Xsd.Boolean", fmap OneOf3 (parseSchemaType "writeAsRead"))
                           , ("Xsd.Boolean", fmap TwoOf3 (parseSchemaType "useEnumeratedValues"))
                           , ("Range", fmap ThreeOf3 (parseSchemaType "range"))
                           ]
    schemaTypeToXML s x@WriteConstraintType{} =
        toXMLElement s []
            [ foldOneOf3  (schemaTypeToXML "writeAsRead")
                          (schemaTypeToXML "useEnumeratedValues")
                          (schemaTypeToXML "range")
                          $ writeConstraintType_choice0 x
            ]
 
data AddressBlockType = AddressBlockType
        { addressBlockType_offset :: ScaledNonNegativeInteger
        , addressBlockType_size :: ScaledNonNegativeInteger
        , addressBlockType_usage :: Xs.Token
        , addressBlockType_protection :: Maybe ProtectionStringType
        }
        deriving (Eq,Show)
instance SchemaType AddressBlockType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return AddressBlockType
            `apply` parseSchemaType "offset"
            `apply` parseSchemaType "size"
            `apply` parseSchemaType "usage"
            `apply` optional (parseSchemaType "protection")
    schemaTypeToXML s x@AddressBlockType{} =
        toXMLElement s []
            [ schemaTypeToXML "offset" $ addressBlockType_offset x
            , schemaTypeToXML "size" $ addressBlockType_size x
            , schemaTypeToXML "usage" $ addressBlockType_usage x
            , maybe [] (schemaTypeToXML "protection") $ addressBlockType_protection x
            ]
 
data InterruptType = InterruptType
        { interruptType_name :: StringType
        , interruptType_description :: Maybe Xsd.XsdString
        , interruptType_value :: Xsd.Integer
        }
        deriving (Eq,Show)
instance SchemaType InterruptType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return InterruptType
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "description")
            `apply` parseSchemaType "value"
    schemaTypeToXML s x@InterruptType{} =
        toXMLElement s []
            [ schemaTypeToXML "name" $ interruptType_name x
            , maybe [] (schemaTypeToXML "description") $ interruptType_description x
            , schemaTypeToXML "value" $ interruptType_value x
            ]
 
 
 
 
 
data CpuType = CpuType
        { cpuType_name :: CpuNameType
        , cpuType_revision :: RevisionType
        , cpuType_endian :: EndianType
        , cpuType_mpuPresent :: Maybe Xsd.Boolean
        , cpuType_fpuPresent :: Maybe Xsd.Boolean
        , cpuType_fpuDP :: Maybe Xsd.Boolean
        , cpuType_icachePresent :: Maybe Xsd.Boolean
        , cpuType_dcachePresent :: Maybe Xsd.Boolean
        , cpuType_itcmPresent :: Maybe Xsd.Boolean
        , cpuType_dtcmPresent :: Maybe Xsd.Boolean
        , cpuType_vtorPresent :: Maybe Xsd.Boolean
        , cpuType_nvicPrioBits :: ScaledNonNegativeInteger
        , cpuType_vendorSystickConfig :: Xsd.Boolean
        , cpuType_deviceNumInterrupts :: Maybe ScaledNonNegativeInteger
        , cpuType_sauNumRegions :: Maybe ScaledNonNegativeInteger
        , cpuType_sauRegionsConfig :: Maybe SauRegionsConfig
        }
        deriving (Eq,Show)
instance SchemaType CpuType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return CpuType
            `apply` parseSchemaType "name"
            `apply` parseSchemaType "revision"
            `apply` parseSchemaType "endian"
            `apply` optional (parseSchemaType "mpuPresent")
            `apply` optional (parseSchemaType "fpuPresent")
            `apply` optional (parseSchemaType "fpuDP")
            `apply` optional (parseSchemaType "icachePresent")
            `apply` optional (parseSchemaType "dcachePresent")
            `apply` optional (parseSchemaType "itcmPresent")
            `apply` optional (parseSchemaType "dtcmPresent")
            `apply` optional (parseSchemaType "vtorPresent")
            `apply` parseSchemaType "nvicPrioBits"
            `apply` parseSchemaType "vendorSystickConfig"
            `apply` optional (parseSchemaType "deviceNumInterrupts")
            `apply` optional (parseSchemaType "sauNumRegions")
            `apply` optional (parseSchemaType "sauRegionsConfig")
    schemaTypeToXML s x@CpuType{} =
        toXMLElement s []
            [ schemaTypeToXML "name" $ cpuType_name x
            , schemaTypeToXML "revision" $ cpuType_revision x
            , schemaTypeToXML "endian" $ cpuType_endian x
            , maybe [] (schemaTypeToXML "mpuPresent") $ cpuType_mpuPresent x
            , maybe [] (schemaTypeToXML "fpuPresent") $ cpuType_fpuPresent x
            , maybe [] (schemaTypeToXML "fpuDP") $ cpuType_fpuDP x
            , maybe [] (schemaTypeToXML "icachePresent") $ cpuType_icachePresent x
            , maybe [] (schemaTypeToXML "dcachePresent") $ cpuType_dcachePresent x
            , maybe [] (schemaTypeToXML "itcmPresent") $ cpuType_itcmPresent x
            , maybe [] (schemaTypeToXML "dtcmPresent") $ cpuType_dtcmPresent x
            , maybe [] (schemaTypeToXML "vtorPresent") $ cpuType_vtorPresent x
            , schemaTypeToXML "nvicPrioBits" $ cpuType_nvicPrioBits x
            , schemaTypeToXML "vendorSystickConfig" $ cpuType_vendorSystickConfig x
            , maybe [] (schemaTypeToXML "deviceNumInterrupts") $ cpuType_deviceNumInterrupts x
            , maybe [] (schemaTypeToXML "sauNumRegions") $ cpuType_sauNumRegions x
            , maybe [] (schemaTypeToXML "sauRegionsConfig") $ cpuType_sauRegionsConfig x
            ]
 
data EnumeratedValueType = EnumeratedValueType
        { enumeratedValueType_name :: IdentifierType
        , enumeratedValueType_description :: Maybe StringType
        , enumeratedValueType_choice2 :: OneOf2 EnumeratedValueDataType Xsd.Boolean
          -- ^ Choice between:
          --   
          --   (1) value
          --   
          --   (2) isDefault
        }
        deriving (Eq,Show)
instance SchemaType EnumeratedValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return EnumeratedValueType
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "description")
            `apply` oneOf' [ ("EnumeratedValueDataType", fmap OneOf2 (parseSchemaType "value"))
                           , ("Xsd.Boolean", fmap TwoOf2 (parseSchemaType "isDefault"))
                           ]
    schemaTypeToXML s x@EnumeratedValueType{} =
        toXMLElement s []
            [ schemaTypeToXML "name" $ enumeratedValueType_name x
            , maybe [] (schemaTypeToXML "description") $ enumeratedValueType_description x
            , foldOneOf2  (schemaTypeToXML "value")
                          (schemaTypeToXML "isDefault")
                          $ enumeratedValueType_choice2 x
            ]
 
data EnumerationType = EnumerationType
        { enumerationType_derivedFrom :: Maybe IdentifierType
        , enumerationType_name :: Maybe IdentifierType
        , enumerationType_headerEnumName :: Maybe IdentifierType
        , enumerationType_usage :: Maybe EnumUsageType
        , enumerationType_enumeratedValue :: [EnumeratedValueType]
        }
        deriving (Eq,Show)
instance SchemaType EnumerationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "derivedFrom" e pos
        commit $ interior e $ return (EnumerationType a0)
            `apply` optional (parseSchemaType "name")
            `apply` optional (parseSchemaType "headerEnumName")
            `apply` optional (parseSchemaType "usage")
            `apply` many1 (parseSchemaType "enumeratedValue")
    schemaTypeToXML s x@EnumerationType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "derivedFrom") $ enumerationType_derivedFrom x
                       ]
            [ maybe [] (schemaTypeToXML "name") $ enumerationType_name x
            , maybe [] (schemaTypeToXML "headerEnumName") $ enumerationType_headerEnumName x
            , maybe [] (schemaTypeToXML "usage") $ enumerationType_usage x
            , concatMap (schemaTypeToXML "enumeratedValue") $ enumerationType_enumeratedValue x
            ]
 
data DimArrayIndexType = DimArrayIndexType
        { dimArrayIndexType_headerEnumName :: Maybe IdentifierType
        , dimArrayIndexType_enumeratedValue :: [EnumeratedValueType]
        }
        deriving (Eq,Show)
instance SchemaType DimArrayIndexType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return DimArrayIndexType
            `apply` optional (parseSchemaType "headerEnumName")
            `apply` many1 (parseSchemaType "enumeratedValue")
    schemaTypeToXML s x@DimArrayIndexType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "headerEnumName") $ dimArrayIndexType_headerEnumName x
            , concatMap (schemaTypeToXML "enumeratedValue") $ dimArrayIndexType_enumeratedValue x
            ]
 
data FieldType = FieldType
        { fieldType_derivedFrom :: Maybe DimableIdentifierType
        , fieldType_dim :: Maybe ScaledNonNegativeInteger
        , fieldType_dimIncrement :: Maybe ScaledNonNegativeInteger
        , fieldType_dimIndex :: Maybe DimIndexType
        , fieldType_dimName :: Maybe IdentifierType
        , fieldType_dimArrayIndex :: Maybe DimArrayIndexType
        , fieldType_name :: DimableIdentifierType
        , fieldType_description :: Maybe StringType
        , fieldType_choice7 :: OneOf3 (ScaledNonNegativeInteger,ScaledNonNegativeInteger) (ScaledNonNegativeInteger,(Maybe (ScaledNonNegativeInteger))) BitRangeType
          -- ^ Choice between:
          --   
          --   (1) Sequence of:
          --   
          --     * lsb
          --   
          --     * msb
          --   
          --   (2) Sequence of:
          --   
          --     * bitOffset
          --   
          --     * bitWidth
          --   
          --   (3) bitRange
        , fieldType_access :: Maybe AccessType
        , fieldType_modifiedWriteValues :: Maybe ModifiedWriteValuesType
        , fieldType_writeConstraint :: Maybe WriteConstraintType
        , fieldType_readAction :: Maybe ReadActionType
        , fieldType_enumeratedValues :: [EnumerationType]
        }
        deriving (Eq,Show)
instance SchemaType FieldType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "derivedFrom" e pos
        commit $ interior e $ return (FieldType a0)
            `apply` optional (parseSchemaType "dim")
            `apply` optional (parseSchemaType "dimIncrement")
            `apply` optional (parseSchemaType "dimIndex")
            `apply` optional (parseSchemaType "dimName")
            `apply` optional (parseSchemaType "dimArrayIndex")
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "description")
            `apply` oneOf' [ ("ScaledNonNegativeInteger ScaledNonNegativeInteger", fmap OneOf3 (return (,) `apply` parseSchemaType "lsb"
                                                                                                           `apply` parseSchemaType "msb"))
                           , ("ScaledNonNegativeInteger Maybe ScaledNonNegativeInteger", fmap TwoOf3 (return (,) `apply` parseSchemaType "bitOffset"
                                                                                                                 `apply` optional (parseSchemaType "bitWidth")))
                           , ("BitRangeType", fmap ThreeOf3 (parseSchemaType "bitRange"))
                           ]
            `apply` optional (parseSchemaType "access")
            `apply` optional (parseSchemaType "modifiedWriteValues")
            `apply` optional (parseSchemaType "writeConstraint")
            `apply` optional (parseSchemaType "readAction")
            `apply` between (Occurs (Just 0) (Just 2))
                            (parseSchemaType "enumeratedValues")
    schemaTypeToXML s x@FieldType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "derivedFrom") $ fieldType_derivedFrom x
                       ]
            [ maybe [] (schemaTypeToXML "dim") $ fieldType_dim x
            , maybe [] (schemaTypeToXML "dimIncrement") $ fieldType_dimIncrement x
            , maybe [] (schemaTypeToXML "dimIndex") $ fieldType_dimIndex x
            , maybe [] (schemaTypeToXML "dimName") $ fieldType_dimName x
            , maybe [] (schemaTypeToXML "dimArrayIndex") $ fieldType_dimArrayIndex x
            , schemaTypeToXML "name" $ fieldType_name x
            , maybe [] (schemaTypeToXML "description") $ fieldType_description x
            , foldOneOf3  (\ (a,b) -> concat [ schemaTypeToXML "lsb" a
                                             , schemaTypeToXML "msb" b
                                             ])
                          (\ (a,b) -> concat [ schemaTypeToXML "bitOffset" a
                                             , maybe [] (schemaTypeToXML "bitWidth") b
                                             ])
                          (schemaTypeToXML "bitRange")
                          $ fieldType_choice7 x
            , maybe [] (schemaTypeToXML "access") $ fieldType_access x
            , maybe [] (schemaTypeToXML "modifiedWriteValues") $ fieldType_modifiedWriteValues x
            , maybe [] (schemaTypeToXML "writeConstraint") $ fieldType_writeConstraint x
            , maybe [] (schemaTypeToXML "readAction") $ fieldType_readAction x
            , concatMap (schemaTypeToXML "enumeratedValues") $ fieldType_enumeratedValues x
            ]
 
data FieldsType = FieldsType
        { fieldsType_field :: [FieldType]
        }
        deriving (Eq,Show)
instance SchemaType FieldsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return FieldsType
            `apply` many1 (parseSchemaType "field")
    schemaTypeToXML s x@FieldsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "field") $ fieldsType_field x
            ]
 
data RegisterType = RegisterType
        { registerType_derivedFrom :: Maybe DimableIdentifierType
        , registerType_dim :: Maybe ScaledNonNegativeInteger
        , registerType_dimIncrement :: Maybe ScaledNonNegativeInteger
        , registerType_dimIndex :: Maybe DimIndexType
        , registerType_dimName :: Maybe IdentifierType
        , registerType_dimArrayIndex :: Maybe DimArrayIndexType
        , registerType_name :: DimableIdentifierType
        , registerType_displayName :: Maybe StringType
        , registerType_description :: Maybe StringType
        , registerType_choice8 :: (Maybe (OneOf2 (Maybe (IdentifierType)) (Maybe (DimableIdentifierType))))
          -- ^ Choice between:
          --   
          --   (1) alternateGroup
          --   
          --   (2) alternateRegister
        , registerType_addressOffset :: ScaledNonNegativeInteger
        , registerType_size :: Maybe ScaledNonNegativeInteger
        , registerType_access :: Maybe AccessType
        , registerType_protection :: Maybe ProtectionStringType
        , registerType_resetValue :: Maybe ScaledNonNegativeInteger
        , registerType_resetMask :: Maybe ScaledNonNegativeInteger
        , registerType_dataType :: Maybe DataTypeType
        , registerType_modifiedWriteValues :: Maybe ModifiedWriteValuesType
        , registerType_writeConstraint :: Maybe WriteConstraintType
        , registerType_readAction :: Maybe ReadActionType
        , registerType_fields :: Maybe FieldsType
        }
        deriving (Eq,Show)
instance SchemaType RegisterType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "derivedFrom" e pos
        commit $ interior e $ return (RegisterType a0)
            `apply` optional (parseSchemaType "dim")
            `apply` optional (parseSchemaType "dimIncrement")
            `apply` optional (parseSchemaType "dimIndex")
            `apply` optional (parseSchemaType "dimName")
            `apply` optional (parseSchemaType "dimArrayIndex")
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "displayName")
            `apply` optional (parseSchemaType "description")
            `apply` optional (oneOf' [ ("Maybe IdentifierType", fmap OneOf2 (optional (parseSchemaType "alternateGroup")))
                                     , ("Maybe DimableIdentifierType", fmap TwoOf2 (optional (parseSchemaType "alternateRegister")))
                                     ])
            `apply` parseSchemaType "addressOffset"
            `apply` optional (parseSchemaType "size")
            `apply` optional (parseSchemaType "access")
            `apply` optional (parseSchemaType "protection")
            `apply` optional (parseSchemaType "resetValue")
            `apply` optional (parseSchemaType "resetMask")
            `apply` optional (parseSchemaType "dataType")
            `apply` optional (parseSchemaType "modifiedWriteValues")
            `apply` optional (parseSchemaType "writeConstraint")
            `apply` optional (parseSchemaType "readAction")
            `apply` optional (parseSchemaType "fields")
    schemaTypeToXML s x@RegisterType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "derivedFrom") $ registerType_derivedFrom x
                       ]
            [ maybe [] (schemaTypeToXML "dim") $ registerType_dim x
            , maybe [] (schemaTypeToXML "dimIncrement") $ registerType_dimIncrement x
            , maybe [] (schemaTypeToXML "dimIndex") $ registerType_dimIndex x
            , maybe [] (schemaTypeToXML "dimName") $ registerType_dimName x
            , maybe [] (schemaTypeToXML "dimArrayIndex") $ registerType_dimArrayIndex x
            , schemaTypeToXML "name" $ registerType_name x
            , maybe [] (schemaTypeToXML "displayName") $ registerType_displayName x
            , maybe [] (schemaTypeToXML "description") $ registerType_description x
            , maybe [] (foldOneOf2  (maybe [] (schemaTypeToXML "alternateGroup"))
                                    (maybe [] (schemaTypeToXML "alternateRegister"))
                                   ) $ registerType_choice8 x
            , schemaTypeToXML "addressOffset" $ registerType_addressOffset x
            , maybe [] (schemaTypeToXML "size") $ registerType_size x
            , maybe [] (schemaTypeToXML "access") $ registerType_access x
            , maybe [] (schemaTypeToXML "protection") $ registerType_protection x
            , maybe [] (schemaTypeToXML "resetValue") $ registerType_resetValue x
            , maybe [] (schemaTypeToXML "resetMask") $ registerType_resetMask x
            , maybe [] (schemaTypeToXML "dataType") $ registerType_dataType x
            , maybe [] (schemaTypeToXML "modifiedWriteValues") $ registerType_modifiedWriteValues x
            , maybe [] (schemaTypeToXML "writeConstraint") $ registerType_writeConstraint x
            , maybe [] (schemaTypeToXML "readAction") $ registerType_readAction x
            , maybe [] (schemaTypeToXML "fields") $ registerType_fields x
            ]
 
data ClusterType = ClusterType
        { clusterType_derivedFrom :: Maybe DimableIdentifierType
        , clusterType_dim :: Maybe ScaledNonNegativeInteger
        , clusterType_dimIncrement :: Maybe ScaledNonNegativeInteger
        , clusterType_dimIndex :: Maybe DimIndexType
        , clusterType_dimName :: Maybe IdentifierType
        , clusterType_dimArrayIndex :: Maybe DimArrayIndexType
        , clusterType_name :: DimableIdentifierType
        , clusterType_description :: Xsd.XsdString
        , clusterType_alternateCluster :: Maybe DimableIdentifierType
        , clusterType_headerStructName :: Maybe IdentifierType
        , clusterType_addressOffset :: ScaledNonNegativeInteger
        , clusterType_size :: Maybe ScaledNonNegativeInteger
        , clusterType_access :: Maybe AccessType
        , clusterType_protection :: Maybe ProtectionStringType
        , clusterType_resetValue :: Maybe ScaledNonNegativeInteger
        , clusterType_resetMask :: Maybe ScaledNonNegativeInteger
        , clusterType_choice15 :: [OneOf2 [RegisterType] [ClusterType]]
          -- ^ Choice between:
          --   
          --   (1) register
          --   
          --   (2) cluster
        }
        deriving (Eq,Show)
instance SchemaType ClusterType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "derivedFrom" e pos
        commit $ interior e $ return (ClusterType a0)
            `apply` optional (parseSchemaType "dim")
            `apply` optional (parseSchemaType "dimIncrement")
            `apply` optional (parseSchemaType "dimIndex")
            `apply` optional (parseSchemaType "dimName")
            `apply` optional (parseSchemaType "dimArrayIndex")
            `apply` parseSchemaType "name"
            `apply` parseSchemaType "description"
            `apply` optional (parseSchemaType "alternateCluster")
            `apply` optional (parseSchemaType "headerStructName")
            `apply` parseSchemaType "addressOffset"
            `apply` optional (parseSchemaType "size")
            `apply` optional (parseSchemaType "access")
            `apply` optional (parseSchemaType "protection")
            `apply` optional (parseSchemaType "resetValue")
            `apply` optional (parseSchemaType "resetMask")
            `apply` many1 (oneOf' [ ("[RegisterType]", fmap OneOf2 (many1 (parseSchemaType "register")))
                                  , ("[ClusterType]", fmap TwoOf2 (many1 (parseSchemaType "cluster")))
                                  ])
    schemaTypeToXML s x@ClusterType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "derivedFrom") $ clusterType_derivedFrom x
                       ]
            [ maybe [] (schemaTypeToXML "dim") $ clusterType_dim x
            , maybe [] (schemaTypeToXML "dimIncrement") $ clusterType_dimIncrement x
            , maybe [] (schemaTypeToXML "dimIndex") $ clusterType_dimIndex x
            , maybe [] (schemaTypeToXML "dimName") $ clusterType_dimName x
            , maybe [] (schemaTypeToXML "dimArrayIndex") $ clusterType_dimArrayIndex x
            , schemaTypeToXML "name" $ clusterType_name x
            , schemaTypeToXML "description" $ clusterType_description x
            , maybe [] (schemaTypeToXML "alternateCluster") $ clusterType_alternateCluster x
            , maybe [] (schemaTypeToXML "headerStructName") $ clusterType_headerStructName x
            , schemaTypeToXML "addressOffset" $ clusterType_addressOffset x
            , maybe [] (schemaTypeToXML "size") $ clusterType_size x
            , maybe [] (schemaTypeToXML "access") $ clusterType_access x
            , maybe [] (schemaTypeToXML "protection") $ clusterType_protection x
            , maybe [] (schemaTypeToXML "resetValue") $ clusterType_resetValue x
            , maybe [] (schemaTypeToXML "resetMask") $ clusterType_resetMask x
            , concatMap (foldOneOf2  (concatMap (schemaTypeToXML "register"))
                                     (concatMap (schemaTypeToXML "cluster"))
                                    ) $ clusterType_choice15 x
            ]
 
data RegistersType = RegistersType
        { registersType_choice0 :: [OneOf2 ClusterType RegisterType]
          -- ^ Choice between:
          --   
          --   (1) cluster
          --   
          --   (2) register
        }
        deriving (Eq,Show)
instance SchemaType RegistersType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return RegistersType
            `apply` many1 (oneOf' [ ("ClusterType", fmap OneOf2 (parseSchemaType "cluster"))
                                  , ("RegisterType", fmap TwoOf2 (parseSchemaType "register"))
                                  ])
    schemaTypeToXML s x@RegistersType{} =
        toXMLElement s []
            [ concatMap (foldOneOf2  (schemaTypeToXML "cluster")
                                     (schemaTypeToXML "register")
                                    ) $ registersType_choice0 x
            ]
 
data PeripheralType = PeripheralType
        { peripheralType_derivedFrom :: Maybe DimableIdentifierType
        , peripheralType_dim :: Maybe ScaledNonNegativeInteger
        , peripheralType_dimIncrement :: Maybe ScaledNonNegativeInteger
        , peripheralType_dimIndex :: Maybe DimIndexType
        , peripheralType_dimName :: Maybe IdentifierType
        , peripheralType_dimArrayIndex :: Maybe DimArrayIndexType
        , peripheralType_name :: DimableIdentifierType
        , peripheralType_version :: Maybe StringType
        , peripheralType_description :: Maybe StringType
        , peripheralType_alternatePeripheral :: Maybe DimableIdentifierType
        , peripheralType_groupName :: Maybe Xs.Name
        , peripheralType_prependToName :: Maybe IdentifierType
        , peripheralType_appendToName :: Maybe IdentifierType
        , peripheralType_headerStructName :: Maybe DimableIdentifierType
        , peripheralType_disableCondition :: Maybe StringType
        , peripheralType_baseAddress :: ScaledNonNegativeInteger
        , peripheralType_size :: Maybe ScaledNonNegativeInteger
        , peripheralType_access :: Maybe AccessType
        , peripheralType_protection :: Maybe ProtectionStringType
        , peripheralType_resetValue :: Maybe ScaledNonNegativeInteger
        , peripheralType_resetMask :: Maybe ScaledNonNegativeInteger
        , peripheralType_addressBlock :: [AddressBlockType]
        , peripheralType_interrupt :: [InterruptType]
        , peripheralType_registers :: Maybe RegistersType
        }
        deriving (Eq,Show)
instance SchemaType PeripheralType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "derivedFrom" e pos
        commit $ interior e $ return (PeripheralType a0)
            `apply` optional (parseSchemaType "dim")
            `apply` optional (parseSchemaType "dimIncrement")
            `apply` optional (parseSchemaType "dimIndex")
            `apply` optional (parseSchemaType "dimName")
            `apply` optional (parseSchemaType "dimArrayIndex")
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "version")
            `apply` optional (parseSchemaType "description")
            `apply` optional (parseSchemaType "alternatePeripheral")
            `apply` optional (parseSchemaType "groupName")
            `apply` optional (parseSchemaType "prependToName")
            `apply` optional (parseSchemaType "appendToName")
            `apply` optional (parseSchemaType "headerStructName")
            `apply` optional (parseSchemaType "disableCondition")
            `apply` parseSchemaType "baseAddress"
            `apply` optional (parseSchemaType "size")
            `apply` optional (parseSchemaType "access")
            `apply` optional (parseSchemaType "protection")
            `apply` optional (parseSchemaType "resetValue")
            `apply` optional (parseSchemaType "resetMask")
            `apply` many (parseSchemaType "addressBlock")
            `apply` many (parseSchemaType "interrupt")
            `apply` optional (parseSchemaType "registers")
    schemaTypeToXML s x@PeripheralType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "derivedFrom") $ peripheralType_derivedFrom x
                       ]
            [ maybe [] (schemaTypeToXML "dim") $ peripheralType_dim x
            , maybe [] (schemaTypeToXML "dimIncrement") $ peripheralType_dimIncrement x
            , maybe [] (schemaTypeToXML "dimIndex") $ peripheralType_dimIndex x
            , maybe [] (schemaTypeToXML "dimName") $ peripheralType_dimName x
            , maybe [] (schemaTypeToXML "dimArrayIndex") $ peripheralType_dimArrayIndex x
            , schemaTypeToXML "name" $ peripheralType_name x
            , maybe [] (schemaTypeToXML "version") $ peripheralType_version x
            , maybe [] (schemaTypeToXML "description") $ peripheralType_description x
            , maybe [] (schemaTypeToXML "alternatePeripheral") $ peripheralType_alternatePeripheral x
            , maybe [] (schemaTypeToXML "groupName") $ peripheralType_groupName x
            , maybe [] (schemaTypeToXML "prependToName") $ peripheralType_prependToName x
            , maybe [] (schemaTypeToXML "appendToName") $ peripheralType_appendToName x
            , maybe [] (schemaTypeToXML "headerStructName") $ peripheralType_headerStructName x
            , maybe [] (schemaTypeToXML "disableCondition") $ peripheralType_disableCondition x
            , schemaTypeToXML "baseAddress" $ peripheralType_baseAddress x
            , maybe [] (schemaTypeToXML "size") $ peripheralType_size x
            , maybe [] (schemaTypeToXML "access") $ peripheralType_access x
            , maybe [] (schemaTypeToXML "protection") $ peripheralType_protection x
            , maybe [] (schemaTypeToXML "resetValue") $ peripheralType_resetValue x
            , maybe [] (schemaTypeToXML "resetMask") $ peripheralType_resetMask x
            , concatMap (schemaTypeToXML "addressBlock") $ peripheralType_addressBlock x
            , concatMap (schemaTypeToXML "interrupt") $ peripheralType_interrupt x
            , maybe [] (schemaTypeToXML "registers") $ peripheralType_registers x
            ]
 
data Device = Device
        { device_schemaVersion :: Xsd.Decimal
        , device_vendor :: Maybe StringType
        , device_vendorID :: Maybe IdentifierType
        , device_name :: IdentifierType
        , device_series :: Maybe StringType
        , device_version :: StringType
        , device_description :: StringType
        , device_licenseText :: Maybe StringType
        , device_cpu :: Maybe CpuType
        , device_headerSystemFilename :: Maybe IdentifierType
        , device_headerDefinitionsPrefix :: Maybe IdentifierType
        , device_addressUnitBits :: ScaledNonNegativeInteger
        , device_width :: ScaledNonNegativeInteger
        , device_size :: Maybe ScaledNonNegativeInteger
        , device_access :: Maybe AccessType
        , device_protection :: Maybe ProtectionStringType
        , device_resetValue :: Maybe ScaledNonNegativeInteger
        , device_resetMask :: Maybe ScaledNonNegativeInteger
        , device_peripherals :: Peripherals
        , device_vendorExtensions :: Maybe VendorExtensions
        }
        deriving (Eq,Show)
instance SchemaType Device where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "schemaVersion" e pos
        commit $ interior e $ return (Device a0)
            `apply` optional (parseSchemaType "vendor")
            `apply` optional (parseSchemaType "vendorID")
            `apply` parseSchemaType "name"
            `apply` optional (parseSchemaType "series")
            `apply` parseSchemaType "version"
            `apply` parseSchemaType "description"
            `apply` optional (parseSchemaType "licenseText")
            `apply` optional (parseSchemaType "cpu")
            `apply` optional (parseSchemaType "headerSystemFilename")
            `apply` optional (parseSchemaType "headerDefinitionsPrefix")
            `apply` parseSchemaType "addressUnitBits"
            `apply` parseSchemaType "width"
            `apply` optional (parseSchemaType "size")
            `apply` optional (parseSchemaType "access")
            `apply` optional (parseSchemaType "protection")
            `apply` optional (parseSchemaType "resetValue")
            `apply` optional (parseSchemaType "resetMask")
            `apply` parseSchemaType "peripherals"
            `apply` optional (parseSchemaType "vendorExtensions")
    schemaTypeToXML s x@Device{} =
        toXMLElement s [ toXMLAttribute "schemaVersion" $ device_schemaVersion x
                       ]
            [ maybe [] (schemaTypeToXML "vendor") $ device_vendor x
            , maybe [] (schemaTypeToXML "vendorID") $ device_vendorID x
            , schemaTypeToXML "name" $ device_name x
            , maybe [] (schemaTypeToXML "series") $ device_series x
            , schemaTypeToXML "version" $ device_version x
            , schemaTypeToXML "description" $ device_description x
            , maybe [] (schemaTypeToXML "licenseText") $ device_licenseText x
            , maybe [] (schemaTypeToXML "cpu") $ device_cpu x
            , maybe [] (schemaTypeToXML "headerSystemFilename") $ device_headerSystemFilename x
            , maybe [] (schemaTypeToXML "headerDefinitionsPrefix") $ device_headerDefinitionsPrefix x
            , schemaTypeToXML "addressUnitBits" $ device_addressUnitBits x
            , schemaTypeToXML "width" $ device_width x
            , maybe [] (schemaTypeToXML "size") $ device_size x
            , maybe [] (schemaTypeToXML "access") $ device_access x
            , maybe [] (schemaTypeToXML "protection") $ device_protection x
            , maybe [] (schemaTypeToXML "resetValue") $ device_resetValue x
            , maybe [] (schemaTypeToXML "resetMask") $ device_resetMask x
            , schemaTypeToXML "peripherals" $ device_peripherals x
            , maybe [] (schemaTypeToXML "vendorExtensions") $ device_vendorExtensions x
            ]
 
elementDevice :: XMLParser Device
elementDevice = parseSchemaType "device"
elementToXMLDevice :: Device -> [Content ()]
elementToXMLDevice = schemaTypeToXML "device"
