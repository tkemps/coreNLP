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

import Control.Lens
import Data.Aeson.Compat
import Data.Aeson.TH
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import GHC.Generics

import CoreNLP.Token (Token)
import CoreNLP.NER
import CoreNLP.THUtil

-- |A representation of a single Sentence. Although it is possible to create a sentence directly from text, it is advisable to create a document instead and operate on the document directly.
data Sentence = Sentence {
    _index :: Int,
    _parse :: Maybe Text,
    _entitymentions :: [EntityMentions],
    _tokens :: [Token]
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''Sentence)
makeLenses ''Sentence

-- |A representation of a Document. Most blobs of raw text should become documents.
data Document = Document {
    _docId :: Maybe Text,
    _sentences :: [Sentence]
} deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = coreNlpFieldLabelModifier} ''Document)
makeLenses ''Document

instance Pretty Document where
    pretty ss = vsep (fmap pretty (ss^.sentences))

instance Pretty Sentence where
    pretty (Sentence{..}) = nest 4 (vsep ["Sentence #" <> pretty _index,
                            pretty _parse,
                            pretty _entitymentions,
                            nest 4 (vsep (fmap pretty _tokens))])

