{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- Xml
-}

module XmlConverter (xmlToDoc, docToXmlString) where

import XmlParser (XmlValue(..), parseXmlFile)
import Types (Doc(..), defaultDoc)
import Data.Maybe (fromMaybe)

xmlValueToDoc :: XmlValue -> Doc
xmlValueToDoc (XmlElement "document" _ children) =
    defaultDoc { isDocument = True }
        { list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "header" attrs children) =
    let title = fromMaybe "" (lookup "title" attrs)
    in defaultDoc { isHeader = True, isTitle = True, string = title,
    list = map xmlValueToDoc children}
xmlValueToDoc (XmlElement "author" _ children) =
    defaultDoc { isAuthor = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "date" _ children) =
    defaultDoc { isDate = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "bold" _ children) =
    defaultDoc { isBold = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "underlined" _ children) =
    defaultDoc { isUnderlined = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "italic" _ children) =
    defaultDoc { isItalic = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "code" _ children) =
    defaultDoc { isCode = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "body" _ children) =
    defaultDoc { isBody = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "paragraph" _ children) =
    defaultDoc { isParagraph = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "section" attrs children) =
    let title = fromMaybe "" (lookup "title" attrs)
    in defaultDoc { isSection = True, isTitle = True, string = title,
    list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "codeblock" _ children) =
    defaultDoc { isCodeBlock = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "list" _ children) =
    defaultDoc { isList = True, list = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "link" attrs children) =
    let url = fromMaybe "" (lookup "url" attrs)
    in defaultDoc { isLink = True, link = url, list
     = map xmlValueToDoc children }
xmlValueToDoc (XmlElement "image" attrs children) =
    let url = fromMaybe "" (lookup "url" attrs)
    in defaultDoc { isImage = True, image = url, list =
         map xmlValueToDoc children }
xmlValueToDoc (XmlText text) =
    defaultDoc { isString = True, string = text }
xmlValueToDoc _ = defaultDoc

xmlToDoc :: String -> Either String Doc
xmlToDoc myString = case parseXmlFile myString of
    Nothing -> Left "Cannot parse"
    Just a -> case xmlValueToDoc a of
        Doc{isDocument = False} -> Left "Cannot parse"
        d -> Right d

docToXmlString :: Doc -> Int -> String
docToXmlString doc indentLvl
    | isDocument doc = i ++ "<document>\n" ++ x ++ i ++ "</document>\n"
    | isHeader doc = i ++ headerToXmlString doc indentLvl
    | isAuthor doc = i ++ "<author>" ++ x ++ "</author>\n"
    | isDate doc = i ++ "<date>" ++ x ++ "</date>\n"
    | isBody doc = i ++ "<body>\n" ++ x ++ i ++ "</body>\n"
    | otherwise = docToXmlString2 doc indentLvl
    where
        i = replicate (indentLvl * 4) ' '
        x = concatMap (\d -> docToXmlString d (indentLvl + 1)) (list doc)

docToXmlString2 :: Doc -> Int -> String
docToXmlString2 doc indentLvl
    | isSection doc = i ++ sectionToXmlString doc indentLvl
    | isParagraph doc = i ++ "<paragraph>" ++ x ++ "</paragraph>\n"
    | isCodeBlock doc = i ++ "<codeblock>\n" ++ x ++ i ++ "</codeblock>\n"
    | isList doc = i ++ "<list>\n" ++ x ++ i ++ "</list>\n"
    | isLink doc = "<link url=\"" ++ link doc ++ "\">" ++ x ++ "</link>"
    | otherwise = docToXmlString3 doc indentLvl
    where
        i = replicate (indentLvl * 4) ' '
        x = concatMap (\d -> docToXmlString d (indentLvl + 1)) (list doc)

docToXmlString3 :: Doc -> Int -> String
docToXmlString3 doc indentLvl
    | isImage doc = "<image url=\"" ++ image doc ++ "\">" ++ x ++ "</image>"
    | isBold doc = "<bold>" ++ x ++ "</bold>"
    | isUnderlined doc = "<underlined>" ++ x ++ "</underlined>"
    | isItalic doc = "<italic>" ++ x ++ "</italic>"
    | isCode doc = "<code>" ++ x ++ "</code>"
    | otherwise = docToXmlString4 doc indentLvl
    where
        x = concatMap (\d -> docToXmlString d (indentLvl + 1)) (list doc)

docToXmlString4 :: Doc -> Int -> String
docToXmlString4 doc _
    | isString doc = string doc
    | otherwise = ""

headerToXmlString :: Doc -> Int -> String
headerToXmlString doc indentLvl =
    let indent = replicate (indentLvl * 4) ' '
        content = concatMap (\d -> docToXmlString d (indentLvl + 1)) (list doc)
    in  "<header title=\"" ++ string doc ++ "\">\n" ++
    content ++ indent ++ "</header>\n"

sectionToXmlString :: Doc -> Int -> String
sectionToXmlString doc indentLvl =
    let indent = replicate (indentLvl * 4) ' '
        content = concatMap (\d -> docToXmlString d (indentLvl + 1)) (list doc)
    in  "<section title=\"" ++ string doc ++ "\">\n" ++ content ++
    indent ++ "</section>\n"
