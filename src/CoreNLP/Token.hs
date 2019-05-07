{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields, RecordWildCards #-}
module CoreNLP.Token where

import Data.Aeson.Compat
import           Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

-- | POS descriptions:
--
--      1.      CC      Coordinating conjunction
--      2.      CD      Cardinal number
--      3.      DT      Determiner
--      4.      EX      Existential there
--      5.      FW      Foreign word
--      6.      IN      Preposition or subordinating conjunction
--      7.      JJ      Adjective
--      8.      JJR     Adjective, comparative
--      9.      JJS     Adjective, superlative
--      10.     LS      List item marker
--      11.     MD      Modal
--      12.     NN      Noun, singular or mass
--      13.     NNS     Noun, plural
--      14.     NNP     Proper noun, singular
--      15.     NNPS    Proper noun, plural
--      16.     PDT     Predeterminer
--      17.     POS     Possessive ending
--      18.     PRP     Personal pronoun
--      19.     PRP$    Possessive pronoun
--      20.     RB      Adverb
--      21.     RBR     Adverb, comparative
--      22.     RBS     Adverb, superlative
--      23.     RP      Particle
--      24.     SYM     Symbol
--      25.     TO      to
--      26.     UH      Interjection
--      27.     VB      Verb, base form
--      28.     VBD     Verb, past tense
--      29.     VBG     Verb, gerund or present participle
--      30.     VBN     Verb, past participle
--      31.     VBP     Verb, non-3rd person singular present
--      32.     VBZ     Verb, 3rd person singular present
--      33.     WDT     Wh-determiner
--      34.     WP      Wh-pronoun
--      35.     WP$     Possessive wh-pronoun
--      36.     WRB     Wh-adverb
data Token = Token {
    index :: Int,
    word :: Text,
    originalText :: Text,
    lemma :: Text,
    characterOffsetBegin :: Int,
    characterOffsetEnd :: Int,
    pos :: Text,
    ner :: Text,
    before :: Text,
    after :: Text}
        deriving (Eq, Show, Generic)

instance ToJSON Token
instance FromJSON Token

instance Pretty Token where
    pretty (Token{..}) = vsep [enclose "[" "]" (pretty index)
                                    <+> pretty word
                                    <+> enclose "(" ")" ("O:" <+> pretty originalText
                                            <+> "L:" <+> pretty lemma),
                               hsep [
                                    "from" <+> pretty characterOffsetBegin,
                                    "to" <+> pretty characterOffsetEnd],
                               "POS"<+> pretty pos,
                               "NER"<+> pretty ner,
                               dquotes (pretty before) <+> ">>"
                                    <+> pretty word <+> ">>"
                                    <+> dquotes (pretty after)
                              ]

