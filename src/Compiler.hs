module Compiler
    ( compile
    ) where

import qualified Data.Text as T
import Text.Parsec
    ( choice, between, try, runParser, many, many1, eof, getState, optional
    , modifyState , option, lookAhead) 
import Text.Parsec.Char
    ( noneOf, char, spaces, letter, oneOf, noneOf, string, digit, hexDigit, anyChar )
import Text.Parsec.Text 
    ( GenParser )
import Data.Char
    ( ord )
import qualified Data.HashMap.Strict as H

data Cell = Literal Int | Label String 
    deriving (Show)
type ParseState = (Int, H.HashMap String Int)
type SParser = GenParser ParseState 

programStart :: Int
programStart = 130

compile :: T.Text -> T.Text
compile programText = result where
    cellState = runParserOrDie programText
    result = T.pack $ show cellState

runParserOrDie :: T.Text -> ([Cell], ParseState)
runParserOrDie source = case runParser file (130, H.empty) "" source of
    Left a -> error . show $ a
    Right b -> b

file :: SParser ([Cell], ParseState)
file = do
    cells <- many line
    eof
    st <- getState
    return (concat $ cells, st)

line :: SParser [Cell]
line = do
    lookAhead anyChar
    optional (try label)
    seps
    cells <- choice
        [ try quotedString
        , try instruction
        , nothing
        ]
    seps
    comment
    modifyState (\(cellNo, hash) ->
        (cellNo + length cells, hash))
    return cells

nothing :: SParser [Cell]
nothing = return []

label :: SParser ()
label = do
    seps
    labelName <- many1 letter
    seps
    char ':'
    modifyState (\(cellNo, hash) ->
        (cellNo, H.insert labelName cellNo hash))

seps1 :: SParser ()
seps1 = do
    many1 (oneOf " \t")
    return ();

seps :: SParser ()
seps = do
    many (oneOf " \t")
    return ()

quotedString :: SParser [Cell]
quotedString = do
    between (char '"') (char '"') (many quotedChar)

quotedChar :: SParser Cell
quotedChar = do
    ch <- choice
        [ do
            char '\\'
            escapeChar
        , noneOf "\"\\"
        ]
    return . Literal . ord $ ch

escapeChar :: SParser Char
escapeChar = choice
    [ do
        char 'n'
        return '\n'
    , do
        char '\"'
        return '\"'
    , do
        char '\\'
        return '\\'
    , do
        char '0'
        return '\0'
    , do
        char 'r'
        return '\r'
    , do
        char 'v'
        return '\v'
    , do
        char 't'
        return '\t'
    , do
        char 'b'
        return '\b'
    , do
        char 'f'
        return '\f'
    ]

instruction :: SParser [Cell]
instruction = do
    (cellNo, _) <- getState
    a <- instructionVal
    seps1
    b <- instructionVal
    c <- try . option (Literal (cellNo + 3)) $ do
        seps1
        instructionVal
    return [a, b, c]

instructionVal :: SParser Cell
instructionVal =  choice
    [ try $ do
        string "0x"
        numString <- many1 hexDigit
        return . Literal . read $ "0x" ++ numString
    , try $ do
        numString <- many1 digit
        return . Literal . read $ numString
    , do
        labelString <- many1 letter
        return . Label $ labelString
    ]

comment :: SParser ()
comment = do
    choice
        [ try $ do
            char ';'
            many (noneOf "\n")
            return ()
        , seps
        ]
    choice 
        [ do
            char '\n'
            return ()
        , eof
        ]