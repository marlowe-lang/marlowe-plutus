module Main (main) where

import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import System.Environment (getArgs)
import Test.Hspec (hspec)

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import PlutusTx.These (These (..))
import qualified PlutusLedgerApi.V1.Address as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.Common as PLC

import Language.Marlowe.Plutus.Binaries.Devel
  ( marloweValidatorBytes
  , marloweValidatorHash
  , rolePayoutValidatorBytes
  , rolePayoutValidatorHash
  )

import Language.Marlowe.Plutus.BinariesSpec (binariesSpec)
import Marlowe.Testing.Scripts
  ( MarloweScripts (..)
  , specForScripts
  , CheckPlutusLog (..)
  , TestFailures (..)
  )
import Marlowe.Testing.ProtocolParams (loadMainnetEvaluationContexts)
import Control.Monad (when)

-- | Construct scripts from in-repo compiled binaries.
develScripts
  :: PV2.MajorProtocolVersion
  -> (PV2.EvaluationContext, PV2.EvaluationContext)
  -> MarloweScripts
develScripts mpv (_ecV2, ecV3) =
  MarloweScripts
    { semanticsAddress = PV1.scriptHashAddress marloweValidatorHash
    , payoutAddress = PV1.scriptHashAddress rolePayoutValidatorHash
    , runSemantics = evaluateBinary mpv ecV3 marloweValidatorBytes
    , runPayout = evaluateBinary mpv ecV3 rolePayoutValidatorBytes
    }

-- | Run full test suite.
runTestSuite :: Maybe (FilePath, FilePath) -> IO ()
runTestSuite maybeFiles = do
  evalCtxs <- loadMainnetEvaluationContexts
  case evalCtxs of
    Left err -> error ("Failed to load protocol params: " <> err)
    Right (mpv, ctxs) -> do
      for_ maybeFiles \(semanticsFile, payoutFile) -> do
        provided <- mkScripts mpv ctxs semanticsFile payoutFile
        scriptsSpec <- specForScripts "Provided" provided (TestFailures True) (CheckPlutusLog False)
        hspec scriptsSpec
      develScriptsSpec <- specForScripts "Devel" (develScripts mpv ctxs) (TestFailures True) (CheckPlutusLog True)
      hspec do
        develScriptsSpec
        binariesSpec

main :: IO ()
main =
  getArgs >>= \case
    semanticsArg : payoutArg : _ ->
      let isFilePath arg = not (null arg) && (fmap fst . List.uncons $ arg) /= Just '-'
       in if isFilePath semanticsArg && isFilePath payoutArg
            then runTestSuite (Just (semanticsArg, payoutArg))
            else runTestSuite Nothing
    _ -> runTestSuite Nothing

-- | Build scripts from external text envelope files.
mkScripts
  :: PV2.MajorProtocolVersion
  -> (PV2.EvaluationContext, PV2.EvaluationContext)
  -> FilePath
  -> FilePath
  -> IO MarloweScripts
mkScripts mpv (ecV2, ecV3) semanticsFile payoutFile = do
  (semanticLang, semanticsBytes, _semanticsHash, semanticsAddr) <- readScript semanticsFile
  (payoutLang, payoutBytes, _payoutHash, payoutAddr) <- readScript payoutFile
  when (semanticLang /= payoutLang) $
    error "Semantics and payout scripts must be the same Plutus version"
  pure $ case semanticLang of
    PLC.PlutusV1 ->
      error "Plutus V1 scripts are not supported for testing"
    PLC.PlutusV2 ->
      MarloweScripts
        { semanticsAddress = semanticsAddr
        , payoutAddress = payoutAddr
        , runSemantics = evaluateBinary mpv ecV2 semanticsBytes
        , runPayout = evaluateBinary mpv ecV2 payoutBytes
        }
    PLC.PlutusV3 ->
      MarloweScripts
        { semanticsAddress = semanticsAddr
        , payoutAddress = payoutAddr
        , runSemantics = evaluateBinary mpv ecV3 semanticsBytes
        , runPayout = evaluateBinary mpv ecV3 payoutBytes
        }

readScript
  :: FilePath
  -> IO (PLC.PlutusLedgerLanguage, PV2.SerialisedScript, PV2.ScriptHash, PV2.Address)
readScript file = do
  possibleResult <-
    readPlutusScriptFile (Proxy @C.PlutusScriptV1) file
      <|> readPlutusScriptFile (Proxy @C.PlutusScriptV2) file
      <|> readPlutusScriptFile (Proxy @C.PlutusScriptV3) file
  (pv, validatorBytes, validatorHash) <-
    case possibleResult of
      Just res -> pure res
      Nothing -> error $ "Failed to read Plutus script from file " ++ file
  let address = PV1.scriptHashAddress validatorHash
  pure (pv, validatorBytes, validatorHash, address)

readPlutusScriptFile
  :: forall lang.
     C.IsPlutusScriptLanguage lang
  => Proxy lang
  -> FilePath
  -> IO (Maybe (PLC.PlutusLedgerLanguage, PV2.SerialisedScript, PV2.ScriptHash))
readPlutusScriptFile _ file = do
  let pv = case C.plutusScriptVersion :: C.PlutusScriptVersion lang of
        C.PlutusScriptV1 -> PLC.PlutusV1
        C.PlutusScriptV2 -> PLC.PlutusV2
        C.PlutusScriptV3 -> PLC.PlutusV3
  C.readFileTextEnvelope (C.File file) <&> \case
    Right (script :: C.Script lang) -> case script of
      C.SimpleScript {} -> Nothing
      C.PlutusScript _ (C.PlutusScriptSerialised bytes) -> do
        let hash = PV2.ScriptHash $ PLC.toBuiltin $ C.serialiseToRawBytes $ C.hashScript script
        Just (pv, bytes, hash)
    Left _ -> Nothing

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
