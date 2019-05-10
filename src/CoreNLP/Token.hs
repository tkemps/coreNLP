{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module CoreNLP.Token where

import Data.Aeson.Compat
import Data.Aeson.TH
import Control.Lens.TH
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.THUtil

-- | CoreNLP splits sentences or texts into tokens. The field POS contains part of speech tags according to the following list:
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
    _index :: Int, -- ^ This indexes a token number inside a sentence. Standardly, tokens are indexed within a sentence starting at 1 (not 0: we follow common parlance whereby we speak of the first word of a sentence). This is generally an individual word or feature index - it is local, and may not be uniquely identifying without other identifiers such as sentence and doc. However, if these are the same, the index annotation should be a unique identifier for differentiating objects.
    _word :: Text, -- ^ The word that forms this token.
    _originalText :: Text, -- ^ Original text from which this token is derived.
    _lemma :: Text, -- ^ Lemmatized form of the token (morphological stem, i.e. simplified to basic form)
    _characterOffsetBegin :: Int, -- ^ Starting position of this token
    _characterOffsetEnd :: Int, -- ^ Ending position of this token
    _pos :: Text, -- ^ POS (part of speech) label
    _ner :: Text, -- ^ Type of NER detected, see 'CoreNLP.NER.EntityMentions' for the NER details.
    _before :: Text, -- ^ The text directly preceding this token -- usually a space.
    _after :: Text -- ^ The text directly after this token -- usually a space, comma or full stop.
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''Token)
makeLenses ''Token

instance Pretty Token where
    pretty (Token{..}) = vsep [enclose "[" "]" (pretty _index)
                                    <+> pretty _word
                                    <+> enclose "(" ")" ("O:" <+> pretty _originalText
                                            <+> "L:" <+> pretty _lemma),
                               hsep [
                                    "from" <+> pretty _characterOffsetBegin,
                                    "to" <+> pretty _characterOffsetEnd],
                               "POS"<+> pretty _pos,
                               "NER"<+> pretty _ner,
                               dquotes (pretty _before) <+> ">>"
                                    <+> pretty _word <+> ">>"
                                    <+> dquotes (pretty _after)
                              ]
