module Marlowe.Binaries.Api.Core where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT(..), throwE, withExceptT)
import Data.Proxy (Proxy (..))
import Marlowe.Testing.Scripts
    ( MarloweScripts(..),
      SemanticsAddress(SemanticsAddress),
      RolePayoutAddress(RolePayoutAddress) )
import PlutusTx.These (These (..))
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified PlutusLedgerApi.Common as PLC
import qualified PlutusLedgerApi.V1.Address as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Marlowe.Testing.ProtocolParams ( EvaluationContexts(..), loadMainnetEvaluationContexts )

type AppM a = ExceptT String IO a

data LoadedScript = LoadedScript
  { address :: PV3.Address
  , bytes :: PV2.SerialisedScript
  , hash :: String
  , language :: PLC.PlutusLedgerLanguage
  , path :: FilePath
  }

readLoadedScript :: FilePath -> AppM LoadedScript
readLoadedScript filePath = do
  readPlutusScriptFile (Proxy @C.PlutusScriptV1) filePath
  <|> readPlutusScriptFile (Proxy @C.PlutusScriptV2) filePath
  <|> readPlutusScriptFile (Proxy @C.PlutusScriptV3) filePath

readPlutusScriptFile
  :: forall lang.
     C.IsPlutusScriptLanguage lang
  => Proxy lang
  -> FilePath
  -> AppM LoadedScript
readPlutusScriptFile _ filePath = do
  (script :: C.Script lang) <-  withExceptT show $ ExceptT $ C.readFileTextEnvelope (C.File filePath)
  case script of
    C.SimpleScript {} -> throwE $ "Expected a Plutus script, but found a simple script in file " <> filePath
    C.PlutusScript _ (C.PlutusScriptSerialised serialisedBytes) -> do
      let
        scriptHash = PV2.ScriptHash $ PLC.toBuiltin $ C.serialiseToRawBytes $ C.hashScript script
      pure $ LoadedScript
        { address = PV1.scriptHashAddress scriptHash
        , bytes = serialisedBytes
        , hash = show scriptHash
        , language = case C.plutusScriptVersion :: C.PlutusScriptVersion lang of
            C.PlutusScriptV1 -> PLC.PlutusV1
            C.PlutusScriptV2 -> PLC.PlutusV2
            C.PlutusScriptV3 -> PLC.PlutusV3
        , path = filePath
        }

-- | Evaluate serialized Plutus script using counting evaluator.
evaluateBinary
  :: PV2.MajorProtocolVersion
  -> PV2.EvaluationContext
  -> PV2.SerialisedScript
  -> PV2.Data
  -> These String PV2.LogOutput
evaluateBinary mpv ec bytes ctx =
  case PV3.deserialiseScript mpv bytes of
    Left err -> This (show err)
    Right script ->
      case PV3.evaluateScriptCounting
             mpv
             PV2.Verbose
             ec
             script
             ctx of
        (logs, Right _) -> That logs
        (logs, Left err)   -> These (show err) logs

-- | Build scripts from external text envelope files.
loadMarloweScripts
  :: FilePath
  -> FilePath
  -> Maybe EvaluationContexts
  -> AppM MarloweScripts
loadMarloweScripts semanticsFile payoutFile possibleEvCtx = do
  EvaluationContexts mpv ecV2 ecV3 <-
    case possibleEvCtx of
      Just evCtx -> pure evCtx
      Nothing -> ExceptT loadMainnetEvaluationContexts
  semanticsScript <- readLoadedScript semanticsFile
  payoutScript <- readLoadedScript payoutFile
  when (semanticsScript.language /= payoutScript.language) $
    throwE "Semantics and payout scripts must be in the same Plutus language"
  case semanticsScript.language of
    PLC.PlutusV1 -> throwE "Plutus V1 scripts are not supported for testing"
    PLC.PlutusV2 -> pure $
      MarloweScripts
        { semanticsAddress = SemanticsAddress semanticsScript.address
        , rolePayoutAddress = RolePayoutAddress payoutScript.address
        , runSemantics = evaluateBinary mpv ecV2 semanticsScript.bytes
        , runPayout = evaluateBinary mpv ecV2 payoutScript.bytes
        }
    PLC.PlutusV3 -> pure $
      MarloweScripts
        { semanticsAddress = SemanticsAddress semanticsScript.address
        , rolePayoutAddress = RolePayoutAddress payoutScript.address
        , runSemantics = evaluateBinary mpv ecV3 semanticsScript.bytes
        , runPayout = evaluateBinary mpv ecV3 payoutScript.bytes
        }


loadMarloweScriptsAddresses
  :: FilePath
  -> FilePath
  -> AppM (SemanticsAddress, RolePayoutAddress)
loadMarloweScriptsAddresses semanticsFile payoutFile = do
  semanticsScript <- readLoadedScript semanticsFile
  payoutScript <- readLoadedScript payoutFile
  when (semanticsScript.language /= payoutScript.language) $
    throwE "Semantics and payout scripts must be in the same Plutus language"
  pure (SemanticsAddress semanticsScript.address, RolePayoutAddress payoutScript.address)
