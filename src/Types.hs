{-# LANGUAGE RecordWildCards #-}
module Types (module Types, AccessType(..), EndianType(..), RevisionType(..)) where

import CMSIS_SVD_1_3_3
    ( AccessType(..)
    , EndianType(..)
    , RevisionType(..)
    , ModifiedWriteValuesType(..)
    , EnumerationType(..)
    , DataTypeType(..)
    )

data Device' = Device'
    { deviceSchemaVersion           :: Double
    , deviceName                    :: String
    , deviceVendor                  :: Maybe String
    , deviceVendorID                :: Maybe String
    , deviceSeries                  :: Maybe String
    , deviceVersion                 :: String
    , deviceDescription             :: String
    , deviceLicenseText             :: Maybe String
    , deviceHeaderSystemFilename    :: Maybe String
    , deviceCPU                     :: Maybe CPU
    , deviceAddressUnitBits         :: Int
    , deviceWidth                   :: Int
    , deviceSize                    :: Maybe Int
    , deviceAccess                  :: Maybe AccessType
    , deviceResetValue              :: Maybe Int
    , deviceResetMask               :: Maybe Int
    , devicePeripherals             :: [Peripheral]
    , deviceVendorExtensions        :: Maybe ()
    , deviceHeaderDefinitionsPrefix :: Maybe String
    } deriving (Eq, Show)

data CPU = CPU
    { cpuName                   :: String
    , cpuRevision               :: RevisionType
    , cpuEndian                 :: EndianType
    , cpuMpuPresent             :: Bool
    , cpuFpuPresent             :: Bool
    , cpuFpuDP                  :: Bool
    , cpuIcachePresent          :: Bool
    , cpuDcachePresent          :: Bool
    , cpuItcmPresent            :: Bool
    , cpuDtcmPresent            :: Bool
    , cpuVtorPresent            :: Bool
    , cpuNvicPrioBits           :: Int
    , cpuVendorSystickConfig    :: Bool
    , cpuDeviceNumInterrupts    :: Maybe Int
    , cpuSauNumRegions          :: Maybe Int
 -- , cpuSauRegionsConfig       :: Maybe ??? -- FIXME: handle this type
    }
    deriving (Eq, Show)

data Peripheral = Peripheral
    { peripheralName                :: String
    , peripheralDerivedFrom         :: Maybe String
    , peripheralDescription         :: String
    , peripheralVersion             :: Maybe String
    , peripheralGroupName           :: Maybe String
    , peripheralPrependToName       :: Maybe String
    , peripheralBaseAddress         :: Int
    , peripheralSize                :: Maybe Int
    , peripheralAddressBlock        :: [AddressBlock]
    , peripheralHeaderStructName    :: Maybe String
    , peripheralInterrupt           :: [Interrupt]
    , peripheralRegisters           :: [Either Cluster Register]
    } deriving (Eq, Show)

type Cluster = ()

data Register = Register
    { registerName          :: String
    , registerDisplayName   :: Maybe String
    , registerDescription   :: String
    , registerAlternative   :: Maybe (Either String String) -- alternateGroup / alternateRegister
    , registerAddressOffset :: Int
    , registerSize          :: Maybe Int
    , registerAccess        :: Maybe AccessType
    , registerResetValue    :: Maybe Int
    , registerResetMask     :: Maybe Int
    , registerModifiedWriteValues   :: Maybe ModifiedWriteValuesType
    , registerDimension     :: Maybe Dimension
    , registerDataType      :: Maybe DataTypeType
    , registerFields        :: [Field]
    } deriving (Eq, Show)

data Field = Field
    { fieldName             :: String
    , fieldDescription      :: String
    , fieldPosition         :: Position
    , fieldAccess           :: Maybe AccessType
    , fieldEnumeratedValues :: [EnumerationType]
    } deriving (Eq, Show)

data Position
    = LsbMsb (Int, Int)
    | OffsetWidth (Int, Maybe Int)
    | BitRange String
    deriving (Eq, Show)

data AddressBlock = AddressBlock
    { addressBlockOffset        :: Int
    , addressBlockSize          :: Int
    , addressBlockUsage         :: String
    , addressBlockProtection    :: Maybe String
    } deriving (Eq, Show)

data Interrupt = Interrupt
    { interruptName         :: String
    , interruptDescription  :: String
    , interruptValue        :: Int
    } deriving (Eq, Show)

newtype Dimension = Dimension (Int, Int, String)   -- (dim, increment, index)
    deriving (Eq, Show)

