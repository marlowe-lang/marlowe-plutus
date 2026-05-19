{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.Next.API (nextApi, NextAPI) where

import Data.Time (UTCTime)
import Marlowe.Plutus.Next (Next)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Servant (OperationId)

import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Servant (
  Description,
  JSON,
  Proxy (..),
  QueryParam',
  QueryParams,
  Required,
  Summary,
  type (:>), UVerb, StdMethod (GET), HasStatus, StatusOf
 )

nextApi :: Proxy NextAPI
nextApi = Proxy

type NextAPI = GETNextContinuationAPI

instance HasStatus Next where
  type StatusOf Next = 200

-- | GET /contracts/:contractId/next/continuation sub-API
type GETNextContinuationAPI =
  Summary "Get next contract steps"
    :> Description "Get inputs which could be performed on a contract withing a time range by the requested parties."
    :> OperationId "getNextStepsForContract"
    :> QueryParam' '[Required, Description "The beginning of the validity range."] "validityStart" UTCTime
    :> QueryParam' '[Required, Description "The end of the validity range."] "validityEnd" UTCTime
    :> QueryParams "party" Party
    :> UVerb
        'GET
        '[JSON]
        '[Next]
