{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-name-shadowing #-}
module Test01Spec (spec) where

import Test.Hspec
import Test.HUnit

import Control.Exception
import Control.Lens
import Control.Monad
import Data.String (IsString(..))
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
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

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = go Set.empty xs
    where
      go s (x:xs)
       | x `Set.member` s = go s xs
       | otherwise        = x : go (Set.insert x s) xs
      go _ _              = []

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

describePOS :: Text -> Text
describePOS pos =
    case Map.lookup pos posDescrs of
        Just d -> d
        Nothing -> T.append "Other: " pos

posDescrs :: Map Text Text
posDescrs = Map.fromList [
    ("CC", "Coordinating conjunction"),
    ("CD", "Cardinal number"),
    ("DT", "Determiner"),
    ("EX", "Existential there"),
    ("FW", "Foreign word"),
    ("IN", "Preposition or subordinating conjunction"),
    ("JJ", "Adjective"),
    ("JJR", "Adjective, comparative"),
    ("JJS", "Adjective, superlative"),
    ("LS", "List item marker"),
    ("MD", "Modal"),
    ("NN", "Noun, singular or mass"),
    ("NNS", "Noun, plural"),
    ("NNP", "Proper noun, singular"),
    ("NNPS", "Proper noun, plural"),
    ("PDT", "Predeterminer"),
    ("POS", "Possessive ending"),
    ("PRP", "Personal pronoun"),
    ("PRP$", "Possessive pronoun"),
    ("RB", "Adverb"),
    ("RBR", "Adverb, comparative"),
    ("RBS", "Adverb, superlative"),
    ("RP", "Particle"),
    ("SYM", "Symbol"),
    ("TO", "to"),
    ("UH", "Interjection"),
    ("VB", "Verb, base form"),
    ("VBD", "Verb, past tense"),
    ("VBG", "Verb, gerund or present participle"),
    ("VBN", "Verb, past participle"),
    ("VBP", "Verb, non-3rd person singular present"),
    ("VBZ", "Verb, 3rd person singular present"),
    ("WDT", "Wh-determiner"),
    ("WP", "Wh-pronoun"),
    ("WP$", "Possessive wh-pronoun"),
    ("WRB", "Wh-adverb")]

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
                        length (x^.sentences) `shouldBe` 6
                it "can find 35 tokens in the first sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        length (x^.sentences^?!ix 0^.tokens) `shouldBe` 35
                it "can find 32 words in the first sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        let poss = x^.sentences^?!ix 0 ^.. tokens . traverse . Tok.pos
                        let words = filter posIsWord poss
                        length words `shouldBe` 32
                it "can find 2 NER (European Commission and EU) in the second sentence" $ \cenv -> do
                    withTextAnalysis cenv someText $ \x -> do
                        let ems = x^.sentences^?! ix 1 ^. entitymentions
                        length ems `shouldBe` 2
                        ems ^?! ix 0 ^. text `shouldBe` "European Commission"
                        ems ^?! ix 1 ^. text `shouldBe` "EU"
            context "when analyzing the first chapter of The Picture of Dorian Gray" $ do
                it "can analyze the text without error" $ \cenv -> do
                    pic01 <- T.readFile "texts/picture01.txt"
                    res <- liftIO cenv $ analyzeEn pic01
                    assertRight ("Result from analyzeText is Left:"++show res) res
                it "can find a lot of POS tags" $ \cenv -> do
                    pic01 <- T.readFile "texts/picture01.txt"
                    withTextAnalysis cenv pic01 $ \x -> do
                        let posTags = sort . nubOrd $ x ^. sentences . traverse . tokens ^.. traverse . Tok.pos
                        forM_ posTags $ \t -> do
                            putStrLn (T.unpack t++" ("++(T.unpack (describePOS t))++")")
    where someText :: Text
          someText = "As the financial crisis evolved into the Euro Area debt crisis it became clear that deeper integration of the banking system was needed for the Euro Area countries, which are particularly interdependent. That’s why, on the basis of the European Commission roadmap for the creation of the banking union, the EU institutions agreed to establish a single supervisory mechanism (SSM) and a single resolution mechanism (SRM) for banks. The banking union applies to countries in the euro area. Non-euro area countries can also join. As a further step to a fully-fledged banking union the Commission put forward a proposal for a European deposit insurance scheme (EDIS) in November 2015. This would provide stronger and more uniform insurance cover for all retail depositors in the banking union."

