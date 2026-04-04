{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Language.Marlowe.Runtime.Core.Api where

import Data.Binary (Binary (..), getWord8, putWord8)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api

newtype ContractId = ContractId {unContractId :: TxOutRef}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary)

data MarloweVersionTag = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

instance Eq (MarloweVersion v) where
  _ == _ = True

instance Ord (MarloweVersion v) where
  compare _ _ = EQ

instance Show (MarloweVersion v) where
  showsPrec _ _ = showString "MarloweV1"

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

instance Eq SomeMarloweVersion where
  SomeMarloweVersion MarloweV1 == SomeMarloweVersion MarloweV1 = True

instance Ord SomeMarloweVersion where
  compare (SomeMarloweVersion MarloweV1) (SomeMarloweVersion MarloweV1) = EQ

instance Bounded SomeMarloweVersion where
  minBound = SomeMarloweVersion MarloweV1
  maxBound = SomeMarloweVersion MarloweV1

instance Enum SomeMarloweVersion where
  toEnum 0 = SomeMarloweVersion MarloweV1
  toEnum _ = error "Invalid marlowe version"
  fromEnum (SomeMarloweVersion MarloweV1) = 0

instance Show SomeMarloweVersion where
  showsPrec p (SomeMarloweVersion v) = showParen (p >= 11) $ showString "SomeMarloweVersion " . showsPrec 11 v

instance Binary SomeMarloweVersion where
  put (SomeMarloweVersion MarloweV1) = putWord8 0x01
  get = getWord8 >>= \case
    0x01 -> pure $ SomeMarloweVersion MarloweV1
    _ -> fail "Invalid marlowe version bytes"
