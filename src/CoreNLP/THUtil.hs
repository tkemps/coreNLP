module CoreNLP.THUtil where

coreNlpFieldLabelModifier :: String -> String
coreNlpFieldLabelModifier = handleTyp . handleUnderscore

handleUnderscore :: String -> String
handleUnderscore fld = if head fld=='_' then tail fld else fld

handleTyp :: String -> String
handleTyp fld =
    if fld=="typ"
    then "type"
    else fld
