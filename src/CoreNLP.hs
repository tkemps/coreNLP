{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module CoreNLP (
    withCoreNLP, analyzeText, mkCoreNlpEnv, liftIO, liftIO', ClientEnv(..), ClientM
) where

import Data.Proxy
import Data.Text (Text)
import qualified Network.HTTP.Client as Net

import Servant.API
import Servant.Client

import CoreNLP.OutputFormat
import CoreNLP.Annotators
import CoreNLP.Sentence

type CoreNlpApi =      QueryParam "annotators" [Annotator]
                    :> QueryParam "tokenize.language" Text
                    :> QueryParam "outputFormat" OutputFormat
                    :> ReqBody '[JSON] Text
                    :> Post '[JSON] Document

coreNlpApi :: Proxy CoreNlpApi
coreNlpApi = Proxy

liftIO' :: ClientEnv -> ClientM a -> IO a
liftIO' cenv = fmap (either (error . show) id)
                . flip runClientM cenv

liftIO :: ClientEnv -> ClientM a -> IO (Either ServantError a)
liftIO cenv = flip runClientM cenv

coreNlpClient :: Maybe [Annotator]
                   -> Maybe Text
                   -> Maybe OutputFormat
                   -> Text
                   -> ClientM Document
coreNlpClient = client coreNlpApi

mkCoreNlpEnv :: String -> Int -> IO ClientEnv
mkCoreNlpEnv url port = do
    manager <- Net.newManager Net.defaultManagerSettings
    let cenv = mkClientEnv manager (BaseUrl Http url port "")
    return cenv

withCoreNLP :: String -> Int -> ClientM a -> IO (Either ServantError a)
withCoreNLP url port act = do
    cenv <- mkCoreNlpEnv url port
    runClientM act cenv

analyzeText :: Maybe [Annotator] -> Maybe Text -> Text -> ClientM Document
analyzeText annotators language text =
    coreNlpClient annotators language (Just JSON) text
