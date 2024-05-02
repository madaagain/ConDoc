{-
-- EPITECH PROJECT, 2024
-- bootstrap pandoc
-- File description:
-- Parser
-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser (Parser(..), parseChar, parseOr, parseAnyChar, parseAnd,
    parseAndWith, parseMany, parseSome, parseUInt, parseInt, parseTuple,
    parseFloat, parseTruple, parseString, fmap, pure, (<*>), (<|>),
    empty, (>>=)) where

import Control.Applicative (Alternative (some, empty, (<|>), many))

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser (\str -> case runParser parser str of
        Nothing -> Nothing
        Just (a, s) -> Just (fct a, s))

instance Applicative Parser where
    pure a = Parser (\str -> Just (a, str))
    (<*>) fct parser = Parser (\str -> case runParser fct str of
        Nothing -> Nothing
        Just (b, s) -> case runParser parser s of
            Nothing -> Nothing
            Just (a, st) -> Just (b a, st))

instance Alternative Parser where
    (<|>) a b = Parser (\str -> case runParser a str of
        Nothing -> runParser b str
        x -> x)
    empty = Parser (const Nothing)

instance Monad Parser where
    (>>=) a f = Parser (\str -> case runParser a str of
        Nothing -> Nothing
        Just (b, s) -> runParser (f b) s)

parseChar :: Char -> Parser Char
parseChar a = Parser (\str -> case str of
    [] -> Nothing
    (x:s) -> case x of
        y | y == a -> Just (a, s)
            | otherwise -> Nothing)

parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (const Nothing)
parseAnyChar [a] = parseChar a
parseAnyChar (a:sx) = parseOr (parseChar a) (parseAnyChar sx)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b = (,) <$> a <*> b

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f a b = f <$> a <*> b

parseMany :: Parser a -> Parser [a]
parseMany = many

parseSome :: Parser a -> Parser [a]
parseSome = some

parseUInt :: Parser Int -- parse an unsigned Int
parseUInt = read <$> parseSome (parseAnyChar "0123456789")

parseInt :: Parser Int -- parse a signed Int
parseInt = (negate <$> (parseChar '-' *> parseUInt)) <|> parseUInt

parseTuple :: Parser a -> Parser (a, a) -- parse a tuple
parseTuple a = do
    parseChar '('
    b <- a
    parseChar ','
    c <- a
    parseChar ')'
    return (b, c)

parseTruple :: Parser (Int, Int, Int)
parseTruple = do
    parseChar '('
    a <- parseInt
    parseChar ','
    b <- parseInt
    parseChar ','
    c <- parseInt
    parseChar ')'
    return (a, b, c)

parseString :: String -> Parser String
parseString = foldr
      (\ a -> (<*>) ((:) <$> parseChar a))
      (Parser (\ str -> Just ([], str)))

parseFloat :: Parser Float
parseFloat = do
    wholePart <- parseInt
    parseChar '.'
    fractionalPart <- parseInt
    let fractionalDigits = length (show fractionalPart)
    return (fromIntegral wholePart +
        fromIntegral fractionalPart / 10 ^ fractionalDigits)
