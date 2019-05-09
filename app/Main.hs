{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions, pretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)

import CoreNLP.Annotators
import CoreNLP

someText :: Text
someText = "As the financial crisis evolved into the euro area debt crisis it became clear that deeper integration of the banking system was needed for the euro area countries, which are particularly interdependent. That’s why, on the basis of the European Commission roadmap for the creation of the banking union, the EU institutions agreed to establish a single supervisory mechanism (SSM) and a single resolution mechanism (SRM) for banks. The banking union applies to countries in the euro area. Non-euro area countries can also join. As a further step to a fully-fledged banking union the Commission put forward a proposal for a European deposit insurance scheme (EDIS) in November 2015. This would provide stronger and more uniform insurance cover for all retail depositors in the banking union."

runCoreNlp :: IO ()
runCoreNlp = do
    res <- withCoreNLP "localhost" 9000 $ do
        analyzeText (Just [Ssplit,Ner,Tokenize, Lemma, Pos]) (Just "en") someText
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right x -> do
            putStrLn (renderString (layoutPretty defaultLayoutOptions (pretty x)))

main :: IO ()
main = do
    putStrLn "CoreNLP via servant"
    runCoreNlp
