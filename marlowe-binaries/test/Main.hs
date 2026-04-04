module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Proxy (Proxy (..))
import System.Environment (getArgs)
import Test.Hspec (hspec)

import qualified Cardano.Api as C
import PlutusTx.These (These (..))
import qualified PlutusLedgerApi.V1.Address as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.Common as PLC
import Control.Monad.Trans.Except (ExceptT(..), withExceptT)

import Language.Marlowe.Plutus.Binaries.Devel
  ( marloweValidatorBytes
  , marloweValidatorHash
  , rolePayoutValidatorBytes
  , rolePayoutValidatorHash
  )

import Language.Marlowe.Plutus.BinariesSpec (binariesSpec)
import Marlowe.Testing.Scripts
  ( MarloweScripts (..)
  , SemanticsAddress(..)
  , RolePayoutAddress(..)
  , specForScripts
  , CheckPlutusLog (..)
  , TestFailures (..)
  )
import Marlowe.Testing.ProtocolParams (EvaluationContexts(..), loadMainnetEvaluationContexts)

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

type AppM a = ExceptT String IO a

-- | Read a Plutus script from a file.
readLoadedScript :: FilePath -> AppM (PV3.Address, PV2.SerialisedScript, PLC.PlutusLedgerLanguage)
readLoadedScript filePath = do
  readPlutusScriptFile (Proxy @C.PlutusScriptV1) filePath
    <|> readPlutusScriptFile (Proxy @C.PlutusScriptV2) filePath
    <|> readPlutusScriptFile (Proxy @C.PlutusScriptV3) filePath

readPlutusScriptFile
  :: forall lang.
     C.IsPlutusScriptLanguage lang
  => Proxy lang
  -> FilePath
  -> AppM (PV3.Address, PV2.SerialisedScript, PLC.PlutusLedgerLanguage)
readPlutusScriptFile _ filePath = do
  (script :: C.Script lang) <- withExceptT show $ ExceptT $ C.readFileTextEnvelope (C.File filePath)
  case script of
    C.SimpleScript {} -> error "Expected a Plutus script"
    C.PlutusScript _ (C.PlutusScriptSerialised serialisedBytes) -> do
      let
        scriptHash = PV2.ScriptHash $ PLC.toBuiltin $ C.serialiseToRawBytes $ C.hashScript script
        address = PV1.scriptHashAddress scriptHash
        language = case C.plutusScriptVersion :: C.PlutusScriptVersion lang of
            C.PlutusScriptV1 -> PLC.PlutusV1
            C.PlutusScriptV2 -> PLC.PlutusV2
            C.PlutusScriptV3 -> PLC.PlutusV3
      pure (address, serialisedBytes, language)

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
  (semanticsAddr, semanticsBytes, semanticsLang) <- readLoadedScript semanticsFile
  (payoutAddr, payoutBytes, payoutLang) <- readLoadedScript payoutFile
  when (semanticsLang /= payoutLang) $
    error "Semantics and payout scripts must be in the same Plutus language"
  case semanticsLang of
    PLC.PlutusV1 -> error "Plutus V1 scripts are not supported for testing"
    PLC.PlutusV2 -> pure $
      MarloweScripts
        { semanticsAddress = SemanticsAddress semanticsAddr
        , rolePayoutAddress = RolePayoutAddress payoutAddr
        , runSemantics = evaluateBinary mpv ecV2 semanticsBytes
        , runPayout = evaluateBinary mpv ecV2 payoutBytes
        }
    PLC.PlutusV3 -> pure $
      MarloweScripts
        { semanticsAddress = SemanticsAddress semanticsAddr
        , rolePayoutAddress = RolePayoutAddress payoutAddr
        , runSemantics = evaluateBinary mpv ecV3 semanticsBytes
        , runPayout = evaluateBinary mpv ecV3 payoutBytes
        }

-- | Construct scripts from in-repo compiled binaries.
develScripts
  :: PV2.MajorProtocolVersion
  -> (PV2.EvaluationContext, PV2.EvaluationContext)
  -> MarloweScripts
develScripts mpv (_ecV2, ecV3) =
  MarloweScripts
    { semanticsAddress = SemanticsAddress $ PV1.scriptHashAddress marloweValidatorHash
    , rolePayoutAddress = RolePayoutAddress $ PV1.scriptHashAddress rolePayoutValidatorHash
    , runSemantics = evaluateBinary mpv ecV3 marloweValidatorBytes
    , runPayout = evaluateBinary mpv ecV3 rolePayoutValidatorBytes
    }

-- | Run full test suite.
runTestSuite :: Maybe (FilePath, FilePath) -> IO ()
runTestSuite maybeFiles = do
  evalCtxs <- loadMainnetEvaluationContexts
  case evalCtxs of
    Left err -> error ("Failed to load protocol params: " <> err)
    Right evCtxs@(EvaluationContexts mpv _ _) -> do
      for_ maybeFiles \(semanticsFile, payoutFile) -> do
        result <- runExceptT $ loadMarloweScripts semanticsFile payoutFile (Just evCtxs)
        case result of
          Left errMsg -> error $ "Failed to load scripts: " <> errMsg
          Right provided -> do
            scriptsSpec <- specForScripts "Provided" provided (TestFailures True) (CheckPlutusLog False)
            hspec scriptsSpec
      let devel = develScripts mpv (evaluationContextFor PLC.PlutusV2 evCtxs, evaluationContextFor PLC.PlutusV3 evCtxs)
      develScriptsSpec <- specForScripts "Devel" devel (TestFailures True) (CheckPlutusLog True)
      hspec do
        develScriptsSpec
        binariesSpec
  where
    evaluationContextFor lang (EvaluationContexts _ ecV2 ecV3) = case lang of
      PLC.PlutusV1 -> error "V1 not supported"
      PLC.PlutusV2 -> ecV2
      PLC.PlutusV3 -> ecV3

main :: IO ()
main =
  getArgs >>= \case
    semanticsArg : payoutArg : _ ->
      let isFilePath arg = not (null arg) && (fmap fst . List.uncons $ arg) /= Just '-'
       in if isFilePath semanticsArg && isFilePath payoutArg
            then runTestSuite (Just (semanticsArg, payoutArg))
            else runTestSuite Nothing
    _ -> runTestSuite Nothing
