module Compiler
    ( compile
    ) where

import qualified Data.Text as T
import Text.Parsec
    ( choice, between, try, runParser, many, many1, eof, getState, optional
    , modifyState , option, lookAhead, sepBy, (<?>)) 
import Text.Parsec.Char
    ( noneOf, char, spaces, letter, oneOf, noneOf, string, digit, hexDigit
    , anyChar, alphaNum)
import Text.Parsec.Text 
    ( GenParser )
import Data.Char
    ( ord )
import Data.Bits
    ( testBit )
import qualified Data.HashMap.Strict as H

data Cell = Literal Int | Label String Int
    deriving (Show)
type LabelMap = H.HashMap String Int
type ParseState = (Int, LabelMap)
type SParser = GenParser ParseState 

programStart :: Int
programStart = 130

compile :: T.Text -> T.Text
compile programText = result where
    (cells, (_, h)) = runParserOrDie programText
    binary = map (intToParagraph . resolveCell h) $ cells    
    body = T.intercalate (T.pack "\n\n") binary
    result = T.concat [cRuntimePrefix, body, cRuntimeSuffix]
    -- result = T.pack . show . map (resolveCell h) $ cells
    -- result = T.pack . show $ cells

    
splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList n xs = (prefix : splitList n suffix) where
    (prefix, suffix) = splitAt n xs
    
intToParagraph :: Int -> T.Text
intToParagraph n = result where
    wStr = T.pack "world"
    hStr = T.pack "hello"

    makeHello True = [hStr]
    makeHello False = [wStr, wStr, wStr, hStr, wStr, wStr]

    stringList = concat . map (makeHello . testBit n) $ [0..31]
    lines = map (T.intercalate (T.pack " ")) . splitList 12 $ stringList
    formattedPara = T.intercalate (T.pack "\n    ") $ lines 
    result  = T.append (T.pack "    ") formattedPara 

resolveCell :: LabelMap -> Cell -> Int
resolveCell _ (Literal a) = a
resolveCell hash (Label s offset) = case H.lookup s hash of
    Just address -> address + offset
    Nothing -> error ("Label has been referenced but not defined: " ++ show s)

runParserOrDie :: T.Text -> ([Cell], ParseState)
runParserOrDie source = case runParser file (130, H.empty) "" source of
    Left a -> error . show $ a
    Right b -> b

file :: SParser ([Cell], ParseState)
file = do
    cells <- many statement
    eof
    st <- getState
    return (concat $ cells, st)

statement :: SParser [Cell]
statement = do
    seps
    choice
        [ try $ do 
            label
            return []
        , notLabel
        ]

notLabel :: SParser [Cell]
notLabel = (do
    cells <- choice
        [ try quotedString
        , try instruction
        , try intData
        , nothing
        ]
    seps
    char ';'
    seps
    modifyState (\(cellNo, hash) ->
        (cellNo + length cells, hash))
    return cells) <?> "statement"

nothing :: SParser [Cell]
nothing = return [] <?> "nothing"

label :: SParser ()
label = (do
    seps
    labelName <- identifier
    seps
    char ':'
    seps
    modifyState (\(cellNo, hash) ->
        (cellNo, H.insert labelName cellNo hash))) <?> "label"

identifier :: SParser String
identifier = (do
    first <- letter
    rest <- many $ choice
        [ alphaNum
        , char '_'
        ]
    return (first : rest)) <?> "identifier"

separator :: SParser ()
separator = choice
    [ (do
        oneOf " \t\n"
        return ()) <?> "whitespace character"
    , comment
    ]

seps1 :: SParser ()
seps1 = do
    many1 separator
    return ()

seps :: SParser ()
seps = do
    many separator
    return ()

quotedString :: SParser [Cell]
quotedString = do
    between (char '"') (char '"') (many quotedChar) <?> "quoted string"

quotedChar :: SParser Cell
quotedChar = (do
    ch <- choice
        [ do
            char '\\'
            escapeChar
        , noneOf "\"\\"
        ]
    return . Literal . (0 -) . ord $ ch) <?> "character literal"

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
    ] <?> "escape character"

instruction :: SParser [Cell]
instruction = (do
    (cellNo, _) <- getState
    a <- unquotedVal
    seps1
    b <- unquotedVal
    c <- option (Literal (cellNo + 3)) (try $ do
        seps1
        unquotedVal )
    return [a, b, c]) <?> "instruction"

unquotedVal :: SParser Cell
unquotedVal =  choice
    [ try $ do
        number <- hexadecimalNumber
        return . Literal $ number
    , try $ do
        number <- decimalNumber
        return . Literal $ number
    , do
        labelReference
    ]

decimalNumber :: SParser Int
decimalNumber = (do
    sign <- option [] $ do 
            char '-'
            return "-"
    numString <- many1 digit
    return . read $ sign ++ numString) <?> "decimal number"

hexadecimalNumber :: SParser Int
hexadecimalNumber = (do
        string "0x"
        numString <- many1 hexDigit
        return . read $ "0x" ++ numString) <?> "hexadecimal number"
 

labelReference :: SParser Cell
labelReference = do
    labelString <- identifier
    offset <- option 0 $ do
        char '.'
        choice
            [ try hexadecimalNumber
            , decimalNumber
            ]
    return (Label labelString offset)

comment :: SParser ()
comment = (do
    char '#'
    many (noneOf "\n")
    choice 
        [ do
            char '\n'
            return ()
        , eof
        ]) <?> "comment"

intData :: SParser [Cell]
intData = between (char '[') (char ']') (
    (do
        seps
        val <- unquotedVal
        seps
        return val) 
    `sepBy` 
        (char ',')) <?> "literal list"

cRuntimePrefix :: T.Text
cRuntimePrefix = T.pack
    "#include <stdio.h>\n\
    \#define hello t(0);\n\
    \#define world t(1);\n\
    \\n\
    \int p=0;int m[8192]={0};int g(int a){return a>=0?m[a]:getchar();}void s(int\n\
    \a,int v){if(a>=0){m[a]=v;}else{putchar(v);}}void t(int i){for(;;){int a=m[p\n\
    \],b=m[p+1],c=m[p+2];switch(i){case 0:{int aD=g(a),bD=g(b),bi=aD%32,ma=1<<bi\n\
    \,nB=ma^g(bD),nA=aD+1,O=ma&nB,mo=nA%32;s(bD,nB);s(a,mo);if(nA!=mo){s(b,g(b)+\n\
    \1);}if(O){i=2;} else{i=3;}};break;case 1:{int aD=g(a),bD=g(b),bi=aD%32,ma=1\n\
    \<<bi,nB=ma^g(bD),O=ma&nB;s(bD,nB);if(O==0){i=2;}else{i=3;}}break;case 2:if(\n\
    \c<0){p+=3;i=4;break;}i=5;p=c;break;case 3:p+=3;i=5;break;case 4:{int aD=g(a\n\
    \),bD=b>=0?m[b]:0,nB=bD-aD;s(b,nB);if(nB<=0){if(c<0){p+=3;i=5;break;}else{p=\n\
    \c;i=4;break;}}else{p+=3;i=4;}}break;case 5:return;}}}\n\
    \\n\
    \int main () {\n\
    \\n\
    \    world hello world world hello world hello world hello world hello world\n\
    \    hello world hello world hello world hello world hello world hello world\n\
    \    hello world hello world hello world hello world hello world hello world\n\
    \    hello world hello world hello world hello world hello world hello world\n\
    \    hello world hello world hello world world hello world world world hello\n\
    \    world hello world world world hello world hello world hello world hello\n\
    \    world world world world world world hello world hello world hello world\n\
    \    world world world hello world hello world hello world world world world\n\
    \    hello world hello world hello world world world world hello world world\n\
    \    world world hello world hello world hello world world world world hello\n\
    \    world hello world world world world world hello world hello world hello\n\
    \    world world world world hello world hello world hello world world world\n\
    \    world hello world hello world hello world hello hello world world hello\n\
    \    world hello world hello world world world\n\
    \    \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    world world world hello world world world world world hello world world \n\
    \    \n\
    \    hello hello hello hello hello hello hello hello hello hello hello hello \n\
    \    hello hello hello hello hello hello hello hello hello hello hello hello \n\
    \    hello hello hello hello hello hello hello hello \n\
    \\n"

cRuntimeSuffix :: T.Text
cRuntimeSuffix = T.pack
    "\n\n\
    \    hello hello hello hello world world hello hello world hello world world \n\
    \    hello world hello world world hello world hello world hello world hello \n\
    \    world hello world hello world hello world hello world hello world hello \n\
    \    world hello world hello world hello world hello world hello world hello \n\
    \    world world  \n\
    \\n\
    \}\n"
