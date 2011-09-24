{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , NoMonomorphismRestriction
           #-}

module HTML where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((*>), (<*))

data Node = Tag String [Node] | Text String deriving Show

instance Lift Node where
    lift (Text t)            = [| Text t |]
    lift (Tag name children) = [| Tag name children |]

html :: QuasiQuoter
html = QuasiQuoter htmlExpr htmlPat undefined undefined

htmlPat :: String -> PatQ
htmlPat "<_>"      = [p| Tag _ children |]
htmlPat "#text"    = [p| Text text |]
htmlPat ('<':rest) = return $
    ConP (mkName "HTML.Tag") [LitP (StringL (init rest)), VarP (mkName "children")]

htmlExpr :: String -> ExpQ
htmlExpr str = do
    filename <- loc_filename `fmap` location
    case parse tagNode filename str of
        Left err -> undefined
        Right tag -> [| tag |]

textNode :: Parser Node
textNode = fmap Text $ many1 $ satisfy (/='<')

tagNode :: Parser Node
tagNode = do
    tagName <- char '<' *> many1 letter <* char '>'
    children <- many $ try tagNode <|> textNode
    string "</" >> string tagName >> char '>'
    return $ Tag tagName children
