{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module CoreNLP.NER where

import Control.Lens
import Data.Aeson.Compat
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.THUtil

data Timex = Timex {
    _tid :: Text,
    _typ :: Text,
    _value :: Maybe Text
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''Timex)
makeLenses ''Timex

data EntityMentions = EntityMentions {
    _docTokenBegin :: Int,
    _docTokenEnd :: Int,
    _tokenBegin :: Int,
    _tokenEnd :: Int,
    _text :: Text,
    _characterOffsetBegin :: Int,
    _characterOffsetEnd :: Int,
    _ner :: Text,
    _normalizedNER :: Maybe Text,
    _timex :: Maybe Timex
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''EntityMentions)
makeLenses ''EntityMentions

instance Pretty EntityMentions where
    pretty em = viaShow em
