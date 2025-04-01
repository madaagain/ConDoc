# Pandoc

The Pandoc project is designed to simplify the complex process of document conversion across various formats, mirroring the functionality of the renowned Pandoc software. This project focuses on the development of a versatile tool capable of transforming documents between different markup languages and file formats, ensuring seamless integration and compatibility across diverse platforms.

The primary goal of the project is to create a simplified version of Pandoc that supports conversion between three specific formats: XML, JSON, and Markdown. This functionality is particularly useful in environments where document exchange and format interoperability are crucial, such as academic publishing, web content management, and software documentation.

Key to the project is the implementation of a custom parsing library developed in Haskell, which is instrumental in interpreting and converting the document structures specific to each format. This library not only facilitates accurate document conversions but also serves as a critical learning tool in functional programming and compiler construction.

The project leverages Haskell's strong type system and functional programming paradigms to ensure that the conversions are not only correct but also efficient in terms of computation and memory usage. This approach aligns with the best practices in software development, particularly in functional programming and system design.


## Features

Convert documents between XML, JSON, and Markdown formats.
Custom parsing library developed specifically for this project.
Command-line interface for easy interaction and integration into other processes.

# Getting Started

## Prerequisites

- Haskell
- Stack `(version 2.1.3 or later, LTS 20 series)`

## Installation

- 1. Clone the repository:
` git clone https://github.com/EpitechPromo2027/B-FUN-400-PAR-4-1-mypandoc-guillaume.deplaine.git`

## Usage

- To run the program, use the following command structure:

`./mypandoc -i [input file] -f [output format] [-o [output file]] [-e [input format]]`

- i: Path to the input file (mandatory)
- f: Format of the output file (XML, JSON, Markdown) (mandatory)
- o: Path to the output file (optional)
- e: Format of the input file (optional)

If no output file is specified, the program outputs to standard output. If the input format is not provided, the program will attempt to detect it.

## Example

`./mypandoc -i example/example.xml -f markdown`

# Documentation Structure

## Document Structure

- Each document is composed of a header and a content body, with the following possible elements:

- Header: May include title, author, and date.
- Content: Consists of text, formatting tags (italic, bold, code), links, images, structural elements (paragraphs, sections), and lists.

## Formats

- The project supports three formats:

- XML: Using standard XML syntax.
- JSON: Lightweight data-interchange format.
- Markdown: Lightweight markup language for creating formatted text.

## Parser

The custom parsing library must be used for implementing parsers for the specified file formats. You may not use any external parsing libraries.

## Documentation

### Big thank for people working with me on this

- [Documentation](https://madsdocs.gitbook.io/documentation-de-pandoc)




