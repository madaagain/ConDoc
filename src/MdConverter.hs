{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- md_converter
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use string literal" #-}
module MdConverter (docToMarkdown) where
import Types(Doc(..))

docToMarkdown :: Doc -> Int -> String
docToMarkdown doc indentLvl
    | isHeader doc = headerToMarkdown doc indentLvl
    | isDocument doc = x
    | isAuthor doc = "author: " ++ x ++ "\n"
    | isDate doc = "date: " ++ x ++ "\n"
    | isBody doc = x
    | isSection doc = sectionToMarkdown doc indentLvl
    | isParagraph doc = "\n" ++ x ++ "\n"
    | isCodeBlock doc = "\n```" ++ x ++ "```\n"
    | isList doc =  listToMarkdown doc indentLvl
    | isLink doc = "[" ++ x ++ "]" ++ "(" ++ link doc ++ ")"
    | isImage doc = " ![" ++ x ++ "]" ++ "(" ++ image doc ++ ")"
    | isBold doc = "**" ++ x ++ "**"
    | isUnderlined doc = "__" ++ x ++ "__"
    | isItalic doc = "*" ++ x ++ "*"
    | isCode doc = "`" ++ x ++ "`"
    | isString doc = string doc
    | otherwise = ""
    where
        x = concatMap (\d -> docToMarkdown d (indentLvl + 1)) (list doc)

listToMarkdown :: Doc -> Int -> String
listToMarkdown doc indentLvl =
    let indent = replicate (indentLvl * 0) ' '
        content = concatMap (\d -> "- " ++ filter (/= '\n')
         (docToMarkdown d (indentLvl + 1)) ++ "\n") (list doc)
    in  content ++ indent


headerToMarkdown :: Doc -> Int -> String
headerToMarkdown doc indentLvl =
    let indent = replicate (indentLvl * 0) ' '
        content = concatMap (\d -> docToMarkdown d (indentLvl + 1)) (list doc)
    in  "---\ntitle: " ++ string doc ++ "\n" ++
    content ++ indent ++ "---" ++ "\n"

sectionToMarkdown :: Doc -> Int -> String
sectionToMarkdown doc indentLvl =
    let indent = replicate (indentLvl * 0) ' '
        hashes = replicate (indentLvl - 1) '#'
        content = concatMap (\d -> docToMarkdown d (indentLvl + 1)) (list doc)
    in  if not (null (string doc)) then  "\n" ++
     hashes ++ " " ++ string doc ++ "\n"
       ++ content ++ indent else content
