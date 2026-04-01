module Marlowe.Binaries.Api.Benchmark
  ( BenchmarkRequest(..)
  , BenchmarkResponse(..)
  , BenchmarkCaseResult(..)
  , BenchmarkScenario(..)
  , BenchmarkScenarioId(..)
  , BenchmarkGenerateRequest(..)
  , BenchmarkGenerateResponse(..)
  , BenchmarkGeneratedSuite(..)
  , EvaluationError(..)
  , ResourceUsage(..)
  , benchmarkScripts
  , generateBenchmarks
  ) where

import Cardano.Crypto.Hash qualified as Hash
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.SatInt (unSatInt)
import GHC.Generics (Generic)
import Marlowe.Testing.ProtocolParams
    ( loadMainnetEvaluationContexts,
      EvaluationContexts,
      EvaluationContexts(..) )
import PlutusLedgerApi.Common qualified as PLC
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import System.Directory
    ( createDirectoryIfMissing, listDirectory )
import Control.Applicative (Alternative((<|>)))
import qualified Data.Aeson as A
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Base16.Aeson (EncodeBase16(..))
import Marlowe.Testing.Semantics.Golden
    ( GoldenTransaction, goldenTransactions )
import Marlowe.Testing.Reference (readReferencePaths, referenceTransactions)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)
import Data.Aeson (ToJSON(..), FromJSON, (.:), withObject, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import Cardano.Crypto.Hash (hashWith)
import Data.Text (Text)
import Data.Traversable (for)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE
import Marlowe.Binaries.Api.Core (LoadedScript(..), readLoadedScript)
import Marlowe.Testing.Scripts
    ( AddNoise(AddNoise),
      genValidSemanticsTransaction,
      mkBareSemanticsTransaction, SemanticsAddress(..), RolePayoutAddress(..) )
import Marlowe.Testing.Contrib.PlutusTx.PlutusTransaction (PlutusTransaction(..))
import Marlowe.Testing.Transactions (SemanticsTransaction)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.Except (ExceptT (ExceptT))

data BenchmarkScenario = BenchmarkScenario
  { metadata :: Text
  , body :: GoldenTransaction
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BenchmarkScenario where
  toJSON BenchmarkScenario{metadata, body} =
    object
      [ "metadata" .= metadata
      , "body" .= body
      ]

instance FromJSON BenchmarkScenario where
  parseJSON = withObject "BenchmarkScenario" $ \obj ->
    BenchmarkScenario
      <$> obj .: "metadata"
      <*> obj .: "body"

newtype BenchmarkScenarioId = BenchmarkScenarioId { bytes :: BS.ByteString }
  deriving stock (Eq, Show, Generic)

deriving via EncodeBase16 instance ToJSON BenchmarkScenarioId
deriving via EncodeBase16 instance FromJSON BenchmarkScenarioId

mkScenarioId :: BenchmarkScenario -> BenchmarkScenarioId
mkScenarioId scenario = do
  let
    json = toJSON scenario.body
    scenarioJsonBytes = BSL.toStrict . A.encode $ json
    scenarioHash :: Hash.Hash Hash.SHA256 BS.ByteString
    scenarioHash = hashWith id scenarioJsonBytes
  BenchmarkScenarioId $ Hash.hashToBytes scenarioHash

scenarioId2Hex :: BenchmarkScenarioId -> Text
scenarioId2Hex (BenchmarkScenarioId bytes) =
  TE.decodeUtf8 . Base16.encode $ bytes

newtype GenSeed = GenSeed { unContextGenSeed :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data BenchmarkMeasurement = BenchmarkMeasurement
  { cpu :: Integer
  , memory :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BenchmarkMeasurement where
  toJSON BenchmarkMeasurement{cpu, memory} =
    object
      [ "cpu" .= cpu
      , "memory" .= memory
      ]

instance FromJSON BenchmarkMeasurement where
  parseJSON = withObject "BenchmarkMeasurement" $ \obj ->
    BenchmarkMeasurement <$> obj .: "cpu" <*> obj .: "memory"

type BenchmarkM a = ExceptT String IO a

exCpu :: PV2.ExBudget -> Integer
exCpu budget = case PV2.exBudgetCPU budget of PV2.ExCPU amount -> toInteger $ unSatInt amount

exMemory :: PV2.ExBudget -> Integer
exMemory budget = case PV2.exBudgetMemory budget of PV2.ExMemory amount -> toInteger $ unSatInt amount

newtype BenchmarkGenerateRequest = BenchmarkGenerateRequest { scenarioRootDir :: FilePath }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data BenchmarkGeneratedSuite = BenchmarkGeneratedSuite
  { generatedScenarioDir :: FilePath
  , generatedScenarios :: [BenchmarkScenarioId]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BenchmarkGeneratedSuite where
  toJSON BenchmarkGeneratedSuite{generatedScenarioDir, generatedScenarios} =
    object
      [ "generatedScenarioDir" .= generatedScenarioDir
      , "generatedScenarios" .= generatedScenarios
      ]

instance FromJSON BenchmarkGeneratedSuite where
  parseJSON = withObject "BenchmarkGeneratedSuite" $ \obj ->
    BenchmarkGeneratedSuite
      <$> obj .: "generatedScenarioDir"
      <*> obj .: "generatedScenarios"

newtype BenchmarkGenerateResponse = BenchmarkGenerateResponse
  { generatedBenchmarkSuites :: [BenchmarkGeneratedSuite]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

generateBenchmarks :: BenchmarkGenerateRequest -> BenchmarkM BenchmarkGenerateResponse
generateBenchmarks req = do
  referencePaths <- liftIO readReferencePaths
  let
    -- We use transactions generated using `getAllInputs` and `computeTransaction`
    -- plus the manually vetted transactions from the golden suite.
    generated = referenceTransactions referencePaths
    goldens = generated <> concat goldenTransactions
    scenarios = BenchmarkScenario "" <$> goldens
  scenariosIds <- for scenarios $ \scenario -> do
    let
      scenarioId = mkScenarioId scenario
      scenarioIdHex = T.unpack $ scenarioId2Hex scenarioId
      scenarioPath = req.scenarioRootDir </> scenarioIdHex <> ".json"
    liftIO $ BSL.writeFile scenarioPath (A.encode scenario)
    pure scenarioId
  pure $ BenchmarkGenerateResponse
    [BenchmarkGeneratedSuite req.scenarioRootDir scenariosIds]

data BenchmarkRequest = BenchmarkRequest
  { benchmarkRootDir :: FilePath
  , payoutScriptFile :: FilePath
  , scenarioRootDir :: FilePath
  , semanticsScriptFile :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BenchmarkRequest where
  toJSON BenchmarkRequest{benchmarkRootDir, semanticsScriptFile, payoutScriptFile, scenarioRootDir} =
    object
      [ "benchmarkRootDir" .= benchmarkRootDir
      , "payoutScriptFile" .= payoutScriptFile
      , "scenarioRootDir" .= scenarioRootDir
      , "semanticsScriptFile" .= semanticsScriptFile
      ]

instance FromJSON BenchmarkRequest where
  parseJSON = withObject "BenchmarkRequest" $ \obj ->
    BenchmarkRequest
      <$> obj .: "benchmarkRootDir"
      <*> obj .: "payoutScriptFile"
      <*> obj .: "scenarioRootDir"
      <*> obj .: "semanticsScriptFile"

data ResourceUsage = ResourceUsage
  { cpu :: Integer
  , memory :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ResourceUsage where
  toJSON ResourceUsage{cpu, memory} =
    object
      [ "cpu" .= cpu
      , "memory" .= memory
      ]

instance FromJSON ResourceUsage where
  parseJSON = withObject "ResourceUsage" $ \obj ->
    ResourceUsage <$> obj .: "cpu" <*> obj .: "memory"

newtype EvaluationLogs = EvaluationLogs { logs :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data EvaluationError
  = OtherError String
  | EvaluationError String EvaluationLogs
  deriving stock (Eq, Show, Generic)

instance ToJSON EvaluationError where
  toJSON (OtherError message) = object
    [ "OtherError" .= message ]
  toJSON (EvaluationError message logs) = object
    [ "EvaluationError" .= object
      [ "message" .= message
      , "logs" .= logs.logs
      ]
    ]

instance FromJSON EvaluationError where
  parseJSON = withObject "EvaluationError" $ \obj ->
    (OtherError <$> obj .: "OtherError") <|> (obj .: "EvaluationError" >>= \errObj ->
      EvaluationError
        <$> errObj .: "message"
        <*> (EvaluationLogs <$> errObj .: "logs")
    )

data BenchmarkCaseResult = BenchmarkCaseResult
  { scenarioId :: BenchmarkScenarioId
  , result :: Either EvaluationError (ResourceUsage, EvaluationLogs)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BenchmarkCaseResult where
  toJSON BenchmarkCaseResult{scenarioId, result} =
    object
      [ "scenarioId" .= scenarioId
      , "result" .= case result of
          Left err -> object [ "error" .= err ]
          Right (resourceUsage, logs) -> object
            [ "resourceUsage" .= resourceUsage
            , "logs" .= logs.logs
            ]
      ]

instance FromJSON BenchmarkCaseResult where
  parseJSON = withObject "BenchmarkCaseResult" $ \obj -> do
    scenarioId <- obj .: "scenarioId"
    result <- (obj .: "result") >>= \resObj -> do
      (Left <$> resObj .: "error")
        <|> do
          resourceUsage <- resObj .: "resourceUsage"
          logs <- resObj .: "logs"
          pure $ Right (resourceUsage, EvaluationLogs logs)
    pure $ BenchmarkCaseResult scenarioId result

newtype BenchmarkResponse = BenchmarkResponse { benchmarkResults :: [BenchmarkCaseResult] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

deterministicGenerate :: GenSeed -> Gen a -> a
deterministicGenerate (GenSeed seed) (MkGen gen) = gen (mkQCGen seed) 30

semanticsContextFromGolden
  :: SemanticsAddress
  -> RolePayoutAddress
  -> GoldenTransaction
  -> PlutusTransaction SemanticsTransaction
semanticsContextFromGolden semanticsAddress rolePayoutAddress golden = do
  let
    seed = GenSeed 42
  deterministicGenerate seed do
    start <- mkBareSemanticsTransaction golden
    genValidSemanticsTransaction semanticsAddress rolePayoutAddress (AddNoise False)
      `execStateT` start

evaluateScript
  :: EvaluationContexts
  -> LoadedScript
  -> PV3.Data
  -> Either EvaluationError (ResourceUsage, EvaluationLogs)
evaluateScript (EvaluationContexts mpv evc2 evc3) loadedScript scriptContextData =
  case PV3.deserialiseScript mpv loadedScript.bytes of
    Left errorMessage -> Left $ OtherError $ show errorMessage
    Right script -> do
      evaluationContext <- case loadedScript.language of
          PLC.PlutusV1 -> Left $ OtherError "Marlowe scripts should not be PlutusV1"
          PLC.PlutusV2 -> pure evc2
          PLC.PlutusV3 -> pure evc3
      let
        (logs, result) = PV3.evaluateScriptCounting mpv PV3.Verbose evaluationContext script scriptContextData
      case result of
        Right budget -> do
          let
            usage = ResourceUsage
              { cpu = exCpu budget
              , memory = exMemory budget
              }
          Right (usage, EvaluationLogs logs)
        Left errorMessage -> Left $ EvaluationError (show errorMessage) $ EvaluationLogs logs

benchmarkScripts :: BenchmarkRequest -> BenchmarkM BenchmarkResponse
benchmarkScripts BenchmarkRequest{..} = do
  semanticsScript <- readLoadedScript semanticsScriptFile
  payoutScript <- readLoadedScript payoutScriptFile
  evaluationContexts <- ExceptT loadMainnetEvaluationContexts

  liftIO $ createDirectoryIfMissing True benchmarkRootDir
  scenarioFiles <- liftIO $ listDirectory scenarioRootDir
  results <- for scenarioFiles $ \scenarioFile -> do
    let scenarioPath = scenarioRootDir </> scenarioFile
    golden <- ExceptT $ Aeson.eitherDecodeFileStrict' scenarioPath
    let
      PlutusTransaction _ ctx = semanticsContextFromGolden
        (SemanticsAddress semanticsScript.address)
        (RolePayoutAddress payoutScript.address)
        golden
      evaluationResult = evaluateScript evaluationContexts semanticsScript (PLC.toData ctx)
      scenarioId = mkScenarioId $ BenchmarkScenario "" golden
    pure $ BenchmarkCaseResult scenarioId evaluationResult
  pure $ BenchmarkResponse results


