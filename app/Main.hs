{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- Main
-}

module Main (main) where
import System.Environment (getArgs)
import Error (errorHandling, Conf(..))
import Json (fromJson, toJson)
import MdConverter (docToMarkdown)
import XmlConverter (xmlToDoc, docToXmlString)
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
    args <- getArgs
    conf <- errorHandling args
    case conf of
        Left e -> putStrLn e >> exitWith (ExitFailure 84)
        Right c -> do
            let outputF = if null(outputFile c) then Nothing else
                    Just (outputFile c)
            let content = inputFile c
            myString <- readFile content
            let y = case inputFormat c of
                    3 -> xmlToDoc myString
                    2 -> fromJson myString
                    _ -> case xmlToDoc myString of
                        Left _ -> fromJson myString
                        Right a -> Right a
            case y of
                Left e -> putStrLn e >> exitWith (ExitFailure 84)
                Right a -> case outputFormat c of
                    1 -> output (docToMarkdown a 0) outputF
                    2 -> output (toJson a) outputF
                    3 -> output (docToXmlString a 0) outputF
                    _ -> putStrLn "Error 2" >> exitWith (ExitFailure 84)

output :: String -> Maybe String -> IO ()
output str Nothing = putStr str
output str (Just file) = writeFile file str
