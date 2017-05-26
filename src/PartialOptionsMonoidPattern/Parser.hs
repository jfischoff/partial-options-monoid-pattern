{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module PartialOptionsMonoidPattern.Parser where
import Data.Monoid
import Options.Applicative
import GHC.Generics
import Data.Yaml (ParseException, FromJSON (..), decodeFileEither, (.:?))
import Data.Aeson (withObject)
import System.Exit
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import System.Environment
import System.Directory

data PartialOptions = PartialOptions
   { poRetryCount    :: Last Int
   , poHost          :: Last String
   , poCharacterCode :: Last (Maybe Char)
   } deriving (Show, Eq)

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty
  mappend x y = PartialOptions
    { poRetryCount    = poRetryCount    x <> poRetryCount    y
    , poHost          = poHost          x <> poHost          y
    , poCharacterCode = poCharacterCode x <> poCharacterCode y
    }

defaultPartialOptions :: PartialOptions
defaultPartialOptions = PartialOptions
  { poRetryCount   = pure 5
  , poHost         = Last Nothing
  , poCharacterCode = pure $ Just 'c'
  }

lastOption :: Parser a -> Parser (Last a)
lastOption parser = fmap Last $ optional parser

partialOptionsParser :: Parser PartialOptions
partialOptionsParser
   =  PartialOptions
  <$> lastOption (option auto (long  "retry-count"))
  <*> lastOption (option str  (long  "host"))
  <*> lastOption
        (  fmap Just (option auto (long "character-code"))
       <|> flag' Nothing (long "no-character-code")
        )

instance FromJSON PartialOptions where
  parseJSON = withObject "FromJSON PartialOptions" $ \obj -> do
    poRetryCount   <- Last <$> obj .:? "retry-count"
    poHost         <- Last <$> obj .:? "host"
    poCharacterCode <- Last <$> obj .:? "character-code"
    return PartialOptions {..}

readPartialOptions :: IO (Either String PartialOptions)
readPartialOptions = do
  let configFilePath = "~/.partial-options-monoid-example/config.yaml"
  exists <- doesFileExist configFilePath
  if exists then
    either (Left . show) Right <$>
      decodeFileEither configFilePath
  else
    return $ Right mempty

readEnvMaybe :: Read a => String -> EitherT String IO (Maybe a)
readEnvMaybe envName = do
  value <- lift (lookupEnv envName)
  case value of
    Nothing -> return Nothing
    Just x  -> maybe (left $ "Could not parse env " ++ envName) return
             $ readMaybe x

readEnv :: Read a => String -> EitherT String IO a
readEnv envName = do
   value <-  maybe (left $ "Missing env " ++ envName) return
         =<< lift (lookupEnv envName)
   maybe (left $ "Could not parse env " ++ envName) return $ readMaybe value

envIsSet :: String -> EitherT String IO ()
envIsSet envName = do
  value <- lift (lookupEnv envName)
  case value of
    Nothing -> left $ "Missing env " ++ envName
    Just _  -> return ()

readEnvPartialOptions :: IO (Either String PartialOptions)
readEnvPartialOptions = runEitherT $ do
  poRetryCount   <- Last <$> readEnvMaybe "PARTIAL_OPTIONS_MONOID_EXAMPLE_RETRY"
  poHost         <- Last <$> readEnvMaybe "PARTIAL_OPTIONS_MONOID_EXAMPLE_HOST"
  poCharacterCode <- Last
                <$> optional
                      (   Just    <$> readEnv  "PARTIAL_OPTIONS_MONOID_EXAMPLE_CHARACTER_CODE"
                      <|> Nothing <$  envIsSet "PARTIAL_OPTIONS_MONOID_EXAMPLE_NO_CHARACTER_CODE"
                      )
  return PartialOptions {..}

mkOptions :: PartialOptions -> Either String Options
mkOptions PartialOptions {..} = do
  oRetryCount    <- maybe (Left "Missing retry count!") pure
                  $ getLast poRetryCount
  oHost          <- maybe (Left "Missing host!" ) pure
                  $ getLast poHost
  oCharacterCode <- maybe (Left "Missing cache seconds count!") pure
                  $ getLast poCharacterCode
  return Options {..}

data Options = Options
  { oRetryCount   :: Int
  , oHost         :: String
  , oCharacterCode :: Maybe Char
  } deriving (Show, Eq)

parseOptions :: IO Options
parseOptions = do
  cmdLineOptions <- execParser $ info partialOptionsParser mempty
  fileOptions    <- either die return =<< readPartialOptions
  envOptions     <- either die return =<< readEnvPartialOptions
  let combinedOptions =  defaultPartialOptions
                      <> cmdLineOptions
                      <> fileOptions
                      <> envOptions
  either die return $ mkOptions combinedOptions