{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine
-- File description:
-- Types
-}

module Types(Doc(..), Link, defaultDoc) where

type Link = String

data Doc = Doc {
    isBold::Bool,
    isUnderlined::Bool,
    isItalic::Bool,
    isTitle::Bool,
    isDate::Bool,
    isAuthor::Bool,
    isCode::Bool,
    isString::Bool,
    string::String,

    isDocument::Bool,
    isBody::Bool,
    isHeader::Bool,
    isSection::Bool,
    isParagraph::Bool,
    isCodeBlock::Bool,
    isList::Bool,
    list::[Doc],

    isLink::Bool,
    link::Link,

    isImage::Bool,
    image::Link
} deriving (Show, Eq);

defaultDoc :: Doc
defaultDoc = Doc{isBold = False, isUnderlined = False, isItalic = False,
    isTitle = False, isCode = False, isImage = False, isAuthor = False,
    isBody = False, isCodeBlock = False, isLink = False, isList = False,
    isDate = False, isString = False, isHeader = False, isParagraph = False,
    isSection = False, isDocument = False, string = "", list = [], link = "",
    image = ""}
