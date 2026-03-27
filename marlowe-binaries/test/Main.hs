{-# LANGUAGE ScopedTypeVariables #-}
module Main (
  main,
) where

import Control.Monad (unless)
import Language.Marlowe.PlutusSpec (ScriptsInfo (..), specForScript)
import System.Environment (getArgs)
import Test.Hspec.Core.Runner (
  defaultConfig,
  evaluateSummary,
  readConfig,
  runSpec,
 )

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified PlutusLedgerApi.V1.Address as P (scriptHashAddress)
import qualified PlutusLedgerApi.V2 as P

import Language.Marlowe.Plutus.Binaries as Devel (
  marloweValidatorBytes,
  marloweValidatorHash,
  rolePayoutValidatorBytes,
  rolePayoutValidatorHash,
  )
import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Proxy (Proxy(..))
import qualified PlutusLedgerApi.Common as P
import qualified Data.List as List

develScripts :: ScriptsInfo
develScripts =
    ScriptsInfo
      { semanticsValidatorBytes = Devel.marloweValidatorBytes
      , semanticsValidatorHash = Devel.marloweValidatorHash
      , semanticsAddress = P.scriptHashAddress Devel.marloweValidatorHash
      , payoutValidatorBytes = Devel.rolePayoutValidatorBytes
      , payoutValidatorHash = Devel.rolePayoutValidatorHash
      , payoutAddress = P.scriptHashAddress Devel.rolePayoutValidatorHash
      , plutusVersion = P.PlutusV3
      }

runTestSuite :: Maybe (FilePath, FilePath) -> [String] -> IO ()
runTestSuite maybeFiles flags = do
  binariesSpec <- case maybeFiles of
    (Just (semanticsFile, payoutFile)) -> do
      providedBinaries <- mkScriptsInfo semanticsFile payoutFile
      pure (specForScript "Provided" providedBinaries)
    Nothing -> pure (pure ())
  config <- readConfig defaultConfig flags
  let combinedSpec = do
        binariesSpec
        specForScript "Devel" develScripts
  summary <- runSpec combinedSpec config
  evaluateSummary summary

-- "USAGE: marlowe-plutus [<semantics validator text envelope file> <payoutValidator text envelope file>] [hspec flags]\nWe assume that file names do not start with '-'."
main :: IO ()
main =
  getArgs
    >>= \case
      -- Check if two first are file paths
      allFlags@(semanticsArg : payoutArg : flags) -> do
        let
          isFilePath arg = not (null arg) && (fmap fst . List.uncons $ arg) /= Just '-'
        if isFilePath semanticsArg && isFilePath payoutArg
          then runTestSuite (Just (semanticsArg, payoutArg)) flags
          else runTestSuite Nothing allFlags
      flags -> runTestSuite Nothing flags

mkScriptsInfo
  :: FilePath
  -> FilePath
  -> IO ScriptsInfo
mkScriptsInfo semanticsFile payoutFile = do
  (plutusVersion, semanticsValidatorBytes, semanticsValidatorHash, semanticsAddress) <- readScript semanticsFile
  (plutusVersionPayout, payoutValidatorBytes, payoutValidatorHash, payoutAddress) <- readScript payoutFile
  unless (plutusVersion == plutusVersionPayout) do
    error "Semantics and payout validators have different Plutus versions. The current test suite requires them to be the same."
  pure ScriptsInfo{..}

readScript
  :: FilePath
  -> IO (P.PlutusLedgerLanguage, P.SerialisedScript, P.ScriptHash, P.Address)
readScript file =
  do
    possibleResult <-
      readPlutusScriptFile (Proxy @C.PlutusScriptV1) file
        <|> readPlutusScriptFile (Proxy @C.PlutusScriptV2) file
        <|> readPlutusScriptFile (Proxy @C.PlutusScriptV3) file
    (pv, validatorBytes, validatorHash) <- case possibleResult of
      Just res -> pure res
      Nothing -> error $ "Failed to read Plutus script from file " ++ file
    let
        address = P.scriptHashAddress validatorHash
    pure (pv, validatorBytes, validatorHash, address)

readPlutusScriptFile :: forall lang. C.IsPlutusScriptLanguage lang => Proxy lang -> FilePath -> IO (Maybe (P.PlutusLedgerLanguage, P.SerialisedScript, P.ScriptHash))
readPlutusScriptFile _ file = do
  let
    pv = case C.plutusScriptVersion :: C.PlutusScriptVersion lang of
      C.PlutusScriptV1 -> P.PlutusV1
      C.PlutusScriptV2 -> P.PlutusV2
      C.PlutusScriptV3 -> P.PlutusV3
  C.readFileTextEnvelope (C.File file) <&> \case
    Right (script :: C.Script lang) -> case script of
      C.SimpleScript {} -> Nothing
      C.PlutusScript _ (C.PlutusScriptSerialised bytes) -> do
        let hash = P.ScriptHash $ P.toBuiltin $ C.serialiseToRawBytes $ C.hashScript script
        Just (pv, bytes, hash)
    Left _ -> Nothing
