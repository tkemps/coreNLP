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
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.Token (Token)
import CoreNLP.NER

-- |A representation of a single Sentence. Although it is possible to create a sentence directly from text, it is advisable to create a document instead and operate on the document directly.
data Sentence = Sentence {
    index :: Int,
    parse :: Maybe Text,
    entitymentions :: [EntityMentions],
    tokens :: [Token]
} deriving (Eq, Show, Generic)

instance ToJSON Sentence
instance FromJSON Sentence

-- |A representation of a Document. Most blobs of raw text should become documents.
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

