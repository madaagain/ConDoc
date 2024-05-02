{-
-- EPITECH PROJECT, 2024
-- bootstrap pandoc
-- File description:
-- XmlParser
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use lambda-case" #-}

module XmlParser
    ( parseXmlFile
    , XmlValue(..)
    ) where

import Parser(Parser(..), (<|>), parseChar, parseMany, parseSome,
    parseAnyChar)
import Control.Monad (void)
import Control.Applicative (optional)
import Data.Maybe (fromMaybe)

data XmlValue = XmlElement String [(String, String)] [XmlValue]
              | XmlText String
              deriving (Show, Eq)

parseXmlFile :: String -> Maybe XmlValue
parseXmlFile xml = case runParser parseXmlDocument xml of
    Just (xmlValue, "") -> Just xmlValue
    _ -> Nothing

parseXmlDocument :: Parser XmlValue
parseXmlDocument = do
    parseMany ignoreParser
    root <- parseXmlElement
    parseMany ignoreParser
    return root

ignoreParser :: Parser Char
ignoreParser = parseAnyChar "\n \t\11\12\13\28\29\30\31\133\160"

skipByName :: String -> Bool
skipByName name = case name of
    "author" -> False
    "date" -> False
    "bold" -> False
    "italic" -> False
    "code" -> False
    "paragraph" -> False
    "link" -> False
    "image" -> False
    _ -> True

skipByEndName :: String -> Bool
skipByEndName name = case name of
    "bold" -> False
    "italic" -> False
    "code" -> False
    "link" -> False
    "image" -> False
    "underlined" -> False
    _ -> True

parseXmlElement :: Parser XmlValue
parseXmlElement = do
    parseMany ignoreParser
    parseChar '<'
    name <- parseName
    attrs <- parseAttributes
    parseChar '>'
    if skipByName name then parseMany ignoreParser else return ""
    contents <- parseContents
    parseEndTag name
    return $ XmlElement name attrs contents

parseEndTag :: String -> Parser ()
parseEndTag name =
    parseChar '<' >>
    parseChar '/' >>
    parseMany ignoreParser >>
    parseString name >>
    parseMany ignoreParser >>
    void (parseChar '>') >>
    (if skipByEndName name
        then void (parseMany ignoreParser)
        else return ())


parseContents :: Parser [XmlValue]
parseContents = parseMany $ parseXmlText <|> parseXmlElement

parseXmlText :: Parser XmlValue
parseXmlText = do
    text <- parseSome $ parseAnyCharExcept "<"
    return $ XmlText text

parseAttributeValue :: Parser (Maybe String)
parseAttributeValue = optional (parseSome $ parseAnyCharExcept "\"")

parseAttributes :: Parser [(String, String)]
parseAttributes = parseMany $ do
    parseMany ignoreParser
    name <- parseName
    parseChar '='
    parseChar '"'
    value <- parseAttributeValue
    parseChar '"'
    return (name, fromMaybe "" value)

parseName :: Parser String
parseName = parseSome $ parseAnyCharExcept " \t\n\r/>="

parseString :: String -> Parser String
parseString str = foldr (\c p -> parseChar c *> p) (pure str) str

parseAnyCharExcept :: String -> Parser Char
parseAnyCharExcept exclusions = satisfy (`notElem` exclusions)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of
    [] -> Nothing
    (x:xs) -> if f x then Just (x, xs) else Nothing
