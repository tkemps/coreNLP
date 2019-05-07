{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields, RecordWildCards #-}
module CoreNLP.Sentence where

import Data.Aeson.Compat
import Data.Aeson.TH
import           Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.Token (Token)
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

data Sentence = Sentence {
    index :: Int,
    parse :: Maybe Text,
    entitymentions :: [EntityMentions],
    tokens :: [Token]
} deriving (Eq, Show, Generic)

instance ToJSON Sentence
instance FromJSON Sentence

data Document = Document {
    docId :: Maybe Text,
    sentences :: [Sentence]
} deriving (Eq, Show, Generic)

instance Pretty Document where
    pretty ss = vsep (fmap pretty (sentences ss))

instance Pretty Sentence where
    pretty (Sentence{..}) = nest 4 (vsep ["Sentence #" <> pretty index,
                            pretty parse,
                            pretty entitymentions,
                            nest 4 (vsep (fmap pretty tokens))])

instance ToJSON Document
instance FromJSON Document

