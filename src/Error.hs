{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- Error handling
-}

module Error
    ( errorHandling, Conf(..), Format, markDown, json, xml
    ) where

import Data.Maybe (fromJust, isNothing)

type Format = Int
type File = String

data Conf = Conf {
    inputFile::File,
    outputFile::File,
    inputFormat::Format,
    outputFormat::Format
} deriving(Show)

markDown::Format
markDown = 1

json::Format
json = 2

xml::Format
xml = 3

sec :: [a] -> a
sec l = head (tail l)

defaultConf :: Conf
defaultConf = Conf "" "" (-1) (-1)

modifConf :: Conf -> String -> String -> Maybe Conf
modifConf c "-i" v = Just
        (Conf v (outputFile c) (inputFormat c) (outputFormat c))
modifConf c "-e" "markdown" = Just
        (Conf (inputFile c) (outputFile c) markDown (outputFormat c))
modifConf c "-e" "xml" = Just
        (Conf (inputFile c) (outputFile c) xml (outputFormat c))
modifConf c "-e" "json" = Just
        (Conf (inputFile c) (outputFile c) json (outputFormat c))
modifConf c "-o" v = Just
        (Conf (inputFile c) v (inputFormat c) (outputFormat c))
modifConf c "-f" "markdown" = Just
        (Conf (inputFile c) (outputFile c) (inputFormat c) markDown)
modifConf c "-f" "xml" = Just
        (Conf (inputFile c) (outputFile c) (inputFormat c) xml)
modifConf c "-f" "json" = Just
        (Conf (inputFile c) (outputFile c) (inputFormat c) json)
modifConf _ _ _ = Nothing


getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf list | length list == 1 = Nothing
    | isNothing (modifConf conf (head list) (sec list)) = Nothing
    | otherwise = getOpts (fromJust (modifConf conf (head list)
        (sec list))) (tail (tail list))

errorHandling :: [String] -> IO(Either String Conf)
errorHandling [] = return (Left "No args given")
errorHandling args
    | length args < 4 = return (Left "Bad args given")
    | isNothing (getOpts defaultConf args) = return (Left "Bad args given")
    | otherwise = return (Right (fromJust (getOpts defaultConf args)))
