{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module JsonParser(parseJsonValue, printJson, JsonValue(..)) where

import Parser(Parser(..), (<|>), parseString, parseFloat, parseChar, parseMany,
    parseAnyChar, parseInt)
import Control.Applicative (optional)

data JsonValue = JsonNull
    | JsonBool Bool
    | JsonNum Float
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show)

indent :: String
indent = "    "

ignoreParser :: Parser Char
ignoreParser = parseAnyChar "\n \t\11\12\13\28\29\30\31\133\160"

parseJsonNull :: Parser JsonValue
parseJsonNull = parseString "null" >> return JsonNull

parseJsonBool :: Parser JsonValue
parseJsonBool = (parseString "true" >> return (JsonBool True)) <|>
    (parseString "false" >> return (JsonBool False))

parseJsonNum :: Parser JsonValue
parseJsonNum = JsonNum <$> (parseFloat <|> (fromIntegral <$> parseInt))

parseJsonStr :: Parser JsonValue
parseJsonStr = JsonString <$> (parseChar '"' *> Parser (Just . span (/= '"'))
    <* parseChar '"')

parseJsonArray :: Parser JsonValue
parseJsonArray = do
    parseChar '[' >> parseMany ignoreParser
    end <- optional (parseChar ']')
    case end of
        Just _ -> return (JsonArray [])
        Nothing -> do
            first <- parseJsonValue
            rest <- parseMany (parseChar ',' *> parseJsonValue)
            parseChar ']'
            return (JsonArray (first : rest))

parsePair :: Parser (String, JsonValue)
parsePair = do
    parseMany ignoreParser >> parseChar '"'
    key <- Parser (Just . span (/= '"'))
    parseChar '"' >> parseMany ignoreParser >> parseChar ':'
    parseMany ignoreParser
    value <- parseJsonValue
    parseMany ignoreParser
    return (key, value)

parsePairs :: Parser [(String, JsonValue)]
parsePairs = do
    parseMany ignoreParser
    end <- optional (parseChar '}')
    case end of
        Just _ -> return []
        Nothing -> do
            first <- parsePair
            rest <- parseMany $ parseChar ',' *> parsePair
            return (first : rest)

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject <$> (parseChar '{' *> parsePairs <* parseChar '}')

parseJsonValue :: Parser JsonValue
parseJsonValue = parseMany ignoreParser *> (parseJsonNull <|> parseJsonBool
    <|> parseJsonNum <|> parseJsonStr <|> parseJsonArray <|> parseJsonObject)
    <* parseMany ignoreParser

makeIndent :: Int -> String
makeIndent 0 = "\n"
makeIndent i = makeIndent (i - 1) ++ indent

printJsonNoIndent :: Int -> JsonValue -> String
printJsonNoIndent _ JsonNull = "null"
printJsonNoIndent _ (JsonBool True) = "true"
printJsonNoIndent _ (JsonBool False) = "false"
printJsonNoIndent _ (JsonNum x) | x == fromIntegral (round x) = show (round x)
    | otherwise = show x
printJsonNoIndent _ (JsonString s) = "\"" ++ s ++ "\""
printJsonNoIndent i (JsonArray a) = "[" ++ makeIndent (i + 1) ++
    foldr (\x acc -> case acc of
    [] -> printJsonNoIndent (i + 1) x
    str -> printJsonNoIndent (i + 1) x ++ "," ++
        makeIndent (i + 1) ++ str) "" a ++ makeIndent i ++"]"
printJsonNoIndent i (JsonObject s) = "{" ++ makeIndent (i + 1) ++
    foldr (\(k,v) acc -> case acc of
    [] -> "\"" ++ k ++ "\": " ++ printJsonNoIndent (i + 1) v
    str -> "\"" ++ k ++ "\": " ++ printJsonNoIndent (i + 1) v ++ "," ++
        makeIndent (i + 1) ++ str) "" s ++ makeIndent i ++ "}"

printJson :: JsonValue -> String
printJson = printJsonNoIndent 0
