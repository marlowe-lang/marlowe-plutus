module Marlowe.Binaries.Api.Compile
  ( compileScripts
  , CompileRequest(..)
  , CompileResponse(..)
  , ScriptName(..)
  , ScriptVariant(..)
  , ScriptOutput(..)
  ) where

import Cardano.Api (File (File), PlutusScriptVersion (PlutusScriptV3), Script (PlutusScript), writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Control.Monad (foldM)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=), (.:), withObject)
import GHC.Generics (Generic)
import Language.Marlowe.Plutus.Binaries.Devel qualified as Devel
import Language.Marlowe.Plutus.Binaries.Production qualified as Production
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V1 (ScriptHash)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data ScriptName
  = MarloweSemantics
  | MarloweRolePayout
  deriving stock (Eq, Show, Generic)

instance ToJSON ScriptName where
  toJSON = \case
    MarloweSemantics -> "marlowe-semantics"
    MarloweRolePayout -> "marlowe-rolepayout"

instance FromJSON ScriptName where
  parseJSON = \case
    "marlowe-semantics" -> pure MarloweSemantics
    "marlowe-rolepayout" -> pure MarloweRolePayout
    other -> fail $ "Expected 'marlowe-semantics' or 'marlowe-rolepayout', got: " <> show other

data ScriptVariant
  = DevelScripts
  | ProductionScripts
  deriving stock (Eq, Show, Generic)

instance ToJSON ScriptVariant where
  toJSON = \case
    DevelScripts -> "devel"
    ProductionScripts -> "production"

instance FromJSON ScriptVariant where
  parseJSON = \case
    "devel" -> pure DevelScripts
    "production" -> pure ProductionScripts
    other -> fail $ "Expected 'devel' or 'production', got: " <> show other

data ScriptOutput = ScriptOutput
  { scriptName :: ScriptName
  , scriptHash :: String
  , scriptFile :: FilePath
  , hashFile :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ScriptOutput where
  toJSON ScriptOutput{scriptName, scriptHash, scriptFile, hashFile} =
    object
      [ "scriptName" .= scriptName
      , "scriptHash" .= scriptHash
      , "scriptFile" .= scriptFile
      , "hashFile" .= hashFile
      ]

instance FromJSON ScriptOutput where
  parseJSON = withObject "ScriptOutput" $ \obj ->
    ScriptOutput
      <$> obj .: "scriptName"
      <*> obj .: "scriptHash"
      <*> obj .: "scriptFile"
      <*> obj .: "hashFile"

data CompileRequest = CompileRequest
  { requestVariant :: ScriptVariant
  , requestOutputDir :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CompileRequest where
  toJSON CompileRequest{requestVariant, requestOutputDir} =
    object
      [ "requestVariant" .= requestVariant
      , "requestOutputDir" .= requestOutputDir
      ]

instance FromJSON CompileRequest where
  parseJSON = withObject "CompileRequest" $ \obj ->
    CompileRequest
      <$> obj .: "requestVariant"
      <*> obj .: "requestOutputDir"

data CompileResponse = CompileResponse
  { responseVariant :: ScriptVariant
  , responseOutputDir :: FilePath
  , responseScripts :: [ScriptOutput]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CompileResponse where
  toJSON CompileResponse{responseVariant, responseOutputDir, responseScripts} =
    object
      [ "responseVariant" .= responseVariant
      , "responseOutputDir" .= responseOutputDir
      , "responseScripts" .= responseScripts
      ]

instance FromJSON CompileResponse where
  parseJSON = withObject "CompileResponse" $ \obj ->
    CompileResponse
      <$> obj .: "responseVariant"
      <*> obj .: "responseOutputDir"
      <*> obj .: "responseScripts"

type CompileError = String

data ScriptArtifacts = ScriptArtifacts
  { name :: ScriptName
  , hash :: ScriptHash
  , bytes :: SerialisedScript
  }

compileScripts :: CompileRequest -> IO (Either CompileError CompileResponse)
compileScripts request@CompileRequest{requestVariant, requestOutputDir} = do
  createDirectoryIfMissing True requestOutputDir
  result <- foldM step (Right []) (artifactsFor requestVariant)
  pure $ do
    scripts <- result
    pure CompileResponse{responseVariant = requestVariant, responseOutputDir = requestOutputDir, responseScripts = scripts}
 where
  step (Left err) _ = pure $ Left err
  step (Right outputs) artifact = do
    output <- writeArtifact request artifact
    pure $ (outputs <>) . pure <$> output

mkScriptBaseName :: ScriptName -> String
mkScriptBaseName = \case
  MarloweSemantics -> "marlowe-semantics"
  MarloweRolePayout -> "marlowe-rolepayout"

writeArtifact :: CompileRequest -> ScriptArtifacts -> IO (Either CompileError ScriptOutput)
writeArtifact CompileRequest{requestOutputDir} ScriptArtifacts{name, hash, bytes} = do
  let baseName = mkScriptBaseName name
      scriptFile = requestOutputDir </> baseName <> ".plutus"
      hashFile = requestOutputDir </> baseName <> ".plutus.hash"
      scriptHash = show hash
  writeResult <-
    writeFileTextEnvelope
      (File scriptFile)
      Nothing
      (PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes))
  case writeResult of
    Left err -> pure . Left $ show err
    Right () -> do
      writeFile hashFile (scriptHash <> "\n")
      pure $ Right ScriptOutput{scriptName = name, scriptHash, scriptFile, hashFile}

artifactsFor :: ScriptVariant -> [ScriptArtifacts]
artifactsFor = \case
  DevelScripts ->
    [ ScriptArtifacts MarloweSemantics Devel.marloweValidatorHash Devel.marloweValidatorBytes
    , ScriptArtifacts MarloweRolePayout Devel.rolePayoutValidatorHash Devel.rolePayoutValidatorBytes
    ]
  ProductionScripts ->
    [ ScriptArtifacts MarloweSemantics Production.marloweValidatorHash Production.marloweValidatorBytes
    , ScriptArtifacts MarloweRolePayout Production.rolePayoutValidatorHash Production.rolePayoutValidatorBytes
    ]
