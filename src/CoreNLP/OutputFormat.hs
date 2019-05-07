{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields, RecordWildCards #-}
module CoreNLP.OutputFormat (OutputFormat(..)) where

import Data.Aeson.Compat
import Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Servant

import CoreNLP.Util

-- |Annotators supported by CoreNLP, see <https://stanfordnlp.github.io/CoreNLP/annotators.html>.
data Annotator = Tokenize | -- ^ Tokenizes the text. This splits the text into roughly “words”, using rules or methods suitable for the language being processed. Sometimes the tokens split up surface words in ways suitable for further NLP-processing, for example “isn’t” becomes “is” and “n’t”. The tokenizer saves the beginning and end character offsets of each token in the input text.
                 Cleanxml | -- ^ Remove xml tokens from the document. May use them to mark sentence ends or to extract metadata.
                 Docdate | -- ^ Allows user to specify dates for documents.
                 Ssplit | -- ^ Splits a sequence of tokens into sentences.
                 Pos | -- ^ Labels tokens with their POS tag. For more details see <http://nlp.stanford.edu/software/tagger.html>.
                 Lemma | -- ^ Generates the word lemmas for all tokens in the corpus.
                 Ner | -- ^ Recognizes named (PERSON, LOCATION, ORGANIZATION, MISC), numerical (MONEY, NUMBER, ORDINAL, PERCENT), and temporal (DATE, TIME, DURATION, SET) entities. Named entities are recognized using a combination of three CRF sequence taggers trained on various corpora, such as ACE and MUC. Numerical entities are recognized using a rule-based system. Numerical entities that require normalization, e.g., dates, are normalized to NormalizedNamedEntityTagAnnotation. For more details on the CRF tagger see this page. Sub-annotators: docdate, regexner, tokensregex, entitymentions, and sutime
                 Entitymentions | -- ^ Group NER tagged tokens together into mentions. Run as part of: ner
                 Regexner | -- ^ Implements a simple, rule-based NER over token sequences using Java regular expressions. The goal of this Annotator is to provide a simple framework to incorporate NE labels that are not annotated in traditional NL corpora. For example, the default list of regular expressions that we distribute in the models file recognizes ideologies (IDEOLOGY), nationalities (NATIONALITY), religions (RELIGION), and titles (TITLE). Here is a simple example of how to use RegexNER. For more complex applications, you might consider TokensRegex.
                 Tokensregex | -- ^ Runs a TokensRegex pipeline within a full NLP pipeline.
                 Parse | -- ^ Provides full syntactic analysis, using both the constituent and the dependency representations. The constituent-based output is saved in TreeAnnotation. We generate three dependency-based outputs, as follows: basic, uncollapsed dependencies, saved in BasicDependenciesAnnotation; collapsed dependencies saved in CollapsedDependenciesAnnotation; and collapsed dependencies with processed coordinations, in CollapsedCCProcessedDependenciesAnnotation. Most users of our parser will prefer the latter representation. For more details on the parser, please see <http://nlp.stanford.edu/software/lex-parser.html>. For more details about the dependencies, please refer to <http://nlp.stanford.edu/software/stanford-dependencies.html>.
                 Depparse | -- ^ rovides a fast syntactic dependency parser. We generate three dependency-based outputs, as follows: basic, uncollapsed dependencies, saved in BasicDependenciesAnnotation; collapsed dependencies saved in CollapsedDependenciesAnnotation; and collapsed dependencies with processed coordinations, in CollapsedCCProcessedDependenciesAnnotation. Most users of our parser will prefer the latter representation. For details about the dependency software, see this page. For more details about dependency parsing in general, see this page.
                 Coref | -- ^ Performs coreference resolution on a document, building links between entity mentions that refer to the same entity. Has a variety of modes, including rule-based, statistical, and neural. Sub-annotators: coref.mention
                 Dcoref | -- ^ Implements both pronominal and nominal coreference resolution. The entire coreference graph (with head words of mentions as nodes) is saved in CorefChainAnnotation. For more details on the underlying coreference resolution algorithm, see this page.
                 Relation | -- ^ Stanford relation extractor is a Java implementation to find relations between two entities. The current relation extraction model is trained on the relation types (except the ‘kill’ relation) and data from the paper Roth and Yih, Global inference for entity and relation identification via a linear programming formulation, 2007, except instead of using the gold NER tags, we used the NER tags predicted by Stanford NER classifier to improve generalization. The default model predicts relations Live_In, Located_In, OrgBased_In, Work_For, and None. For more details of how to use and train your own model, see this page
                 Natlog | -- ^ Marks quantifier scope and token polarity, according to natural logic semantics. Places an OperatorAnnotation on tokens which are quantifiers (or other natural logic operators), and a PolarityAnnotation on all tokens in the sentence.
                 Openie | -- ^ Extract open-domain relation triples. System description in the paper <https://nlp.stanford.edu/pubs/2015angeli-openie.pdf>.
                 Entitylink | -- ^ Link entity mentions to Wikipedia entities
                 Kbp | -- ^ Extracts (subject, relation, object) triples from sentences, using a combination of a statistical model, patterns over tokens, and patterns over dependencies. Extracts TAC-KBP relations. Details about models and rules can be found in our write up for the TAC-KBP 2016 competition.
                 Quote | -- ^ Deterministically picks out quotes delimited by “ or ‘ from a text. All top-level quotes are supplied by the top level annotation for a text. If a QuotationAnnotation corresponds to a quote that contains embedded quotes, these quotes will appear as embedded QuotationAnnotations that can be accessed from the QuotationAnnotation that they are embedded in. The QuoteAnnotator can handle multi-line and cross-paragraph quotes, but any embedded quotes must be delimited by a different kind of quotation mark than its parents. Does not depend on any other annotators. Support for unicode quotes is not yet present. Sub-annotators: quote.attribution
                 QuoteAttribution | -- ^ Attribute quotes to speakers in the document. Run as part of: quote
                 Sentiment | -- ^ Implements Socher et al’s sentiment model. Attaches a binarized tree of the sentence to the sentence level CoreMap. The nodes of the tree then contain the annotations from RNNCoreAnnotations indicating the predicted class and scores for that subtree. See the sentiment page for more information about this project.
                 Truecase | -- ^ ecognizes the true case of tokens in text where this information was lost, e.g., all upper case text. This is implemented with a discriminative model implemented using a CRF sequence tagger. The true case label, e.g., INIT_UPPER is saved in TrueCaseAnnotation. The token text adjusted to match its true case is saved as TrueCaseTextAnnotation.
                 Udfeats -- ^ Labels tokens with their Universal Dependencies universal part of speech (UPOS) and features.
    deriving (Eq, Show, Read, Generic)

instance ToJSON Annotator
instance FromJSON Annotator

instance ToHttpApiData Annotator where
    toQueryParam QuoteAttribution = "quote attribution"
    toQueryParam a = T.toLower . T.pack . show $ a

instance FromHttpApiData Annotator where
    parseQueryParam t = if t=="quote attribution"
                        then Right QuoteAttribution
                        else read . firstToUpper . T.unpack $ t

instance ToHttpApiData [Annotator] where
    toQueryParam as = T.intercalate "," (fmap toQueryParam as)

instance FromHttpApiData [Annotator] where
    parseQueryParam t = let es :: [Either Text Annotator]
                            es = fmap parseQueryParam (T.splitOn "," (T.replace ", " "," t))
                            ls = lefts es
                            rs = rights es
                        in if null ls then Right rs else Left (T.intercalate "; " ls)

data OutputFormat = JSON
    deriving (Eq, Show, Read, Generic)

instance FromHttpApiData OutputFormat where
    parseQueryParam = read . firstToUpper . T.unpack

instance ToHttpApiData OutputFormat where
    toQueryParam = T.toLower . T.pack . show

instance ToJSON OutputFormat
instance FromJSON OutputFormat

