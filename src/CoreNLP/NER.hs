{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module CoreNLP.NER where

import Data.Aeson.Compat
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.THUtil

data Timex = Timex {
    tid :: Text,
    typ :: Text,
    value :: Text
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''Timex)

data EntityMentions = EntityMentions {
    docTokenBegin :: Int,
    docTokenEnd :: Int,
    tokenBegin :: Int,
    tokenEnd :: Int,
    text :: Text,
    characterOffsetBegin :: Int,
    characterOffsetEnd :: Int,
    ner :: Text,
    normalizedNER :: Maybe Text,
    timex :: Maybe Timex
} deriving (Eq, Show, Generic)

instance ToJSON EntityMentions
instance FromJSON EntityMentions

instance Pretty EntityMentions where
    pretty em = viaShow em
