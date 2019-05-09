{-# LANGUAGE OverloadedStrings #-}
module Test01Spec (spec) where

import Test.Hspec
--import Test.Hspec.QuickCheck
--import Test.Hspec.Contrib.HUnit
--import Test.QuickCheck
import Test.HUnit

import Control.Exception
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc (Pretty(..), layoutPretty, defaultLayoutOptions, pretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)

import CoreNLP.Annotators
import CoreNLP.NER
import qualified CoreNLP.Token as Tok
import CoreNLP.Sentence
import CoreNLP

assertRight :: String -> Either a b -> IO ()
assertRight _ (Right _) = return ()
assertRight msg (Left _) = assertFailure msg

analyzeEn :: Text -> ClientM Document
analyzeEn = analyzeText (Just [Ssplit, Ner, Tokenize, Lemma, Pos]) (Just "en")

withCoreNLPCEnv :: (ClientEnv -> IO ()) -> IO ()
withCoreNLPCEnv = bracket (mkCoreNlpEnv "localhost" 9000) (\_ -> return ())

withRight :: Show a => Either a b -> (b -> IO c) -> IO c
withRight res tst = do
   case res of
        Right x -> tst x
        Left err -> assertFailure ("Result from analyzeText is Left:"++show err)

withTextAnalysis :: ClientEnv -> Text -> (Document -> IO b) -> IO b
withTextAnalysis cenv txt tst = do
    res <- liftIO cenv $ analyzeEn txt
    withRight res tst

pprint :: Pretty a => a -> IO ()
pprint x = putStrLn (renderString (layoutPretty defaultLayoutOptions (pretty x)))

posIsWord :: (Eq a, IsString a) => a -> Bool
posIsWord "``" = False
posIsWord "," = False
posIsWord "." = False
posIsWord ":" = False
posIsWord _ = True

spec :: Spec
spec = do
    around withCoreNLPCEnv $ do
        describe "coreNLP" $ do
            context ("when analyzing a business text with 6 sentences: "++T.unpack someText) $ do
                it "can analyze the text without error" $ \cenv -> do
                    res <- liftIO cenv $ analyzeEn someText
                    assertRight ("Result from analyzeText is Left:"++show res) res
                it "can split the text into 6 sentences" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        length (sentences x) `shouldBe` 6
                it "can find 35 tokens in the first sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        let s0 = (sentences x)!!0
                        length (tokens s0) `shouldBe` 35
                it "can find 32 words in the first sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        let s0 = (sentences x)!!0
                        let ws = filter (\t -> posIsWord (Tok.pos t)) (tokens s0)
                        length ws `shouldBe` 32
                it "can find 2 NER (European Commission and EU) in the second sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        let s1 = (sentences x)!!1
                            ems = entitymentions s1
                        length ems `shouldBe` 2
                        text (ems!!0) `shouldBe` "European Commission"
                        text (ems!!1) `shouldBe` "EU"
            context "when analyzing the first chapter of The Picture of Dorian Gray" $ do
                it "can analyze the text without error" $ \cenv -> do
                    pic01 <- T.readFile "texts/picture01.txt"
                    res <- liftIO cenv $ analyzeEn pic01
                    assertRight ("Result from analyzeText is Left:"++show res) res
    where someText :: Text
          someText = "As the financial crisis evolved into the Euro Area debt crisis it became clear that deeper integration of the banking system was needed for the Euro Area countries, which are particularly interdependent. That’s why, on the basis of the European Commission roadmap for the creation of the banking union, the EU institutions agreed to establish a single supervisory mechanism (SSM) and a single resolution mechanism (SRM) for banks. The banking union applies to countries in the euro area. Non-euro area countries can also join. As a further step to a fully-fledged banking union the Commission put forward a proposal for a European deposit insurance scheme (EDIS) in November 2015. This would provide stronger and more uniform insurance cover for all retail depositors in the banking union."

{-

 -}
