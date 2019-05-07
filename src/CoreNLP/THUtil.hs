module CoreNLP.THUtil where

coreNlpFieldLabelModifier :: String -> String
coreNlpFieldLabelModifier fld =
    if fld=="typ"
    then "type"
    else fld
