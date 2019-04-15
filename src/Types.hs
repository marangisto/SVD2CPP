{-# LANGUAGE RecordWildCards #-}
module Types (module Types, AccessType(..), EndianType(..), RevisionType(..)) where

import CMSIS_SVD_1_3_3 (AccessType(..), EndianType(..), RevisionType(..), ModifiedWriteValuesType(..))

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
    } deriving (Eq, Show)

data CPU = CPU
    { cpuName                   :: String
    , cpuRevision               :: RevisionType
    , cpuEndian                 :: EndianType
    , cpuMpuPresent             :: Maybe Bool   -- FIXME: consider removing Maybe on these
    , cpuFpuPresent             :: Maybe Bool
    , cpuFpuDP                  :: Maybe Bool
    , cpuIcachePresent          :: Maybe Bool
    , cpuDcachePresent          :: Maybe Bool
    , cpuItcmPresent            :: Maybe Bool
    , cpuDtcmPresent            :: Maybe Bool
    , cpuVtorPresent            :: Maybe Bool
    , cpuNvicPrioBits           :: Int
    , cpuVendorSystickConfig    :: Bool
    , cpuDeviceNumInterrupts    :: Maybe Int
    , cpuSauNumRegions          :: Maybe Int
    , cpuSauRegionsConfig       :: Maybe Int
    }
    deriving (Eq, Show)

data Peripheral = Peripheral
    { peripheralName            :: String
    , peripheralDerivedFrom     :: Maybe String
    , peripheralDescription     :: String
    , peripheralVersion         :: Maybe String
    , peripheralGroupName       :: Maybe String
    , peripheralPrependToName   :: Maybe String
    , peripheralBaseAddress     :: Int
    , peripheralSize            :: Maybe Int
    , peripheralAddressBlock    :: [AddressBlock]
    , peripheralInterrupt       :: [Interrupt]
    , peripheralRegisters       :: [Either Cluster Register]
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
    , registerFields        :: [Field]
    } deriving (Eq, Show)

data Field = Field
    { fieldName         :: String
    , fieldDescription  :: String
    , fieldPosition     :: Position
    , fieldAccess       :: Maybe AccessType
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

