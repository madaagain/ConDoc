{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- Json
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Json(fromJson, toJson) where

import JsonParser(parseJsonValue, printJson, JsonValue(..))
import Types(Doc(..), defaultDoc, Link)
import Parser (Parser(..))
import Data.Either (partitionEithers)
import Data.List (findIndex)
import Data.Maybe (fromJust)

mapEitherList :: (a -> Either String b) -> [a] -> Either String [b]
mapEitherList f s = case partitionEithers (map f s) of
    ([], l) -> Right l
    (errs, _) -> Left (unlines errs)

fromJsonObjectToImage :: [(String, JsonValue)] -> Either String (Link, String)
fromJsonObjectToImage [("url", JsonString a), ("alt", JsonArray b)] = case
    mapEitherList toDoc b of
        Left s -> Left s
        Right [k] -> Right (a, string k)
        Right _ -> Left "Too much alt text for image"
fromJsonObjectToImage [("alt", JsonArray b), ("url", JsonString a)] =case
    mapEitherList toDoc b of
        Left s -> Left s
        Right [k] -> Right (a, string k)
        Right _ -> Left "Too much alt text for image"
fromJsonObjectToImage _ = Left "Unknown Image"

fromJsonObjectToLink :: [(String, JsonValue)] -> Either String (Link, String)
fromJsonObjectToLink [("url", JsonString a), ("content", JsonArray b)] =case
    mapEitherList toDoc b of
        Left s -> Left s
        Right [k] -> Right (a, string k)
        Right _ -> Left "Too much alt text for image"
fromJsonObjectToLink [("content", JsonArray b), ("url", JsonString a)] =case
    mapEitherList toDoc b of
        Left s -> Left s
        Right [k] -> Right (a, string k)
        Right _ -> Left "Too much alt text for image"
fromJsonObjectToLink _ = Left "Unknown Link"

headerDoc :: [(String, JsonValue)] -> Either String Doc
headerDoc [] = Right defaultDoc
headerDoc (x:xs) = do
    f <- headerDoc xs
    doc <- fromJsonObject [x]
    case doc of
        Doc{isTitle = True, list = [Doc{string = s}]} ->
            Right (f {isHeader = True, string = s})
        Doc{isAuthor = True} ->
            Right (f {isHeader = True, list = doc : list f})
        Doc{isDate = True} -> Right (f {isHeader = True, list = doc : list f})
        _ -> Left "Unknown information in header"

fromJsonObject :: [(String, JsonValue)] -> Either String Doc
fromJsonObject [] = Right defaultDoc
fromJsonObject (("header", JsonObject a):l) = do
    k <- fromJsonObject l
    z <- headerDoc a
    case z of
        Doc{isHeader = True, string = []} -> Left "No title found"
        Doc{isHeader = True, string = _} -> Right (k {list = z : list k})
        _ -> Left "No informations is header"
fromJsonObject (("body", JsonArray a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> case mapEitherList toDoc a of
        Left y -> Left y
        Right z -> Right (k {isDocument = True, list = list k ++ [
            defaultDoc{isBody = True, list = z}]})
fromJsonObject (("italic", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isItalic = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("bold", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isBold = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("title", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isTitle = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("author", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isAuthor = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("date", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isDate = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("code", JsonString a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> Right (k {isCode = True, list = [defaultDoc {isString = True,
        string = a}]})
fromJsonObject (("section", JsonObject [("title", JsonString s), ("content",
    JsonArray a)]):l) = case fromJsonObject l of
    Left str -> Left str
    Right k -> case mapEitherList toDoc a of
        Left y -> Left y
        Right z -> Right (k {isSection = True, string = s, list = z})
fromJsonObject (("list", JsonArray a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> case mapEitherList toDoc a of
        Left y -> Left y
        Right z -> Right (k {isList = True, list = z})
fromJsonObject (("codeblock", JsonArray a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> case mapEitherList toDoc a of
        Left y -> Left y
        Right z -> Right (k {isCodeBlock = True, list = z})
fromJsonObject (("link", JsonObject a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> case fromJsonObjectToLink a of
        Left y -> Left y
        Right (z, s) -> Right (k {isLink = True,
            list = [defaultDoc{isString = True, string = s}], link = z})
fromJsonObject (("image", JsonObject a):l) = case fromJsonObject l of
    Left s -> Left s
    Right k -> case fromJsonObjectToImage a of
        Left y -> Left y
        Right (z, s) -> Right (k {isImage = True,
            list = [defaultDoc{isString = True, string = s}], image = z})
fromJsonObject ((key, _):_) = Left (key ++ " is not parsable")

toDoc :: JsonValue -> Either String Doc
toDoc JsonNull = Right defaultDoc{isString = True, string = "null"}
toDoc (JsonBool a) = Right defaultDoc{isString = True, string = show a}
toDoc (JsonNum a) = Right defaultDoc{isString = True, string = show a}
toDoc (JsonArray s) = case mapEitherList toDoc s of
    Left a -> Left a
    Right l -> Right defaultDoc{isParagraph = True, list = l}
toDoc (JsonString a) = Right defaultDoc{isString = True, string = a}
toDoc (JsonObject l) = fromJsonObject l

fromJson :: String -> Either String Doc
fromJson s = maybe (Left "Cannot parse document") (\a -> case a of
    (b, []) -> toDoc b
    _ -> Left "Cannot parse document") (runParser parseJsonValue s)

headerParts :: [Doc] -> [(String, JsonValue)]
headerParts [] = []
headerParts (Doc {isAuthor = True, list = t}: l) = ("author",
    foldl addString (JsonString "") (map docToJson t)) : headerParts l
headerParts (Doc {isDate = True, list = t}: l) = ("date",
    foldl addString (JsonString "") (map docToJson t)) : headerParts l
headerParts (_ : l) = headerParts l

addString :: JsonValue -> JsonValue -> JsonValue
addString (JsonString s) (JsonString k) = JsonString (s ++ k)
addString _ (JsonString k) = JsonString k
addString (JsonString s) _ = JsonString s
addString _ _ = JsonString ""

docToJson :: Doc -> JsonValue
docToJson (Doc {isCodeBlock = True, list = l}) =
    JsonObject [("codeblock", JsonArray (map docToJson l))]
docToJson (Doc {isCode = True, list = l}) =
    JsonObject [("code", foldl addString (JsonString "") (map docToJson l))]
docToJson (Doc {isDocument = True, list = l}) =
    JsonObject [("header", docToJson (l !! fromJust (findIndex isHeader l))),
        ("body", docToJson (l !! fromJust (findIndex isBody l)))]
docToJson (Doc {isBody = True, list = l}) = JsonArray (map docToJson l)
docToJson (Doc {isHeader = True, list = l, string = s}) = JsonObject
    (("title", JsonString s) : headerParts l)
docToJson (Doc {isParagraph = True, list = l}) = JsonArray (map docToJson l)
docToJson (Doc {isList = True, list = l}) =
    JsonObject [("list", JsonArray (map docToJson l))]
docToJson (Doc {isSection = True, list = l, string = s}) = JsonObject
    [("section", JsonObject [("title", JsonString s), ("content",
        JsonArray (map docToJson l))])]
docToJson (Doc {isItalic = True, list = l}) =
    JsonObject [("italic", foldl addString (JsonString "") (map docToJson l))]
docToJson (Doc {isBold = True, list = l}) =
    JsonObject [("bold", foldl addString (JsonString "") (map docToJson l))]
docToJson (Doc {isLink = True, link = url, list = [Doc{string = s}]}) =
    JsonObject [("link", JsonObject [("url", JsonString url), ("content",
        JsonArray [JsonString s])])]
docToJson (Doc {isImage = True, image = url, list = [Doc{string = s}]}) =
    JsonObject [("image", JsonObject [("url", JsonString url), ("alt",
        JsonArray [JsonString s])])]
docToJson (Doc {string = s}) = JsonString s

toJson :: Doc -> String
toJson d = printJson (docToJson d)
