-- HTMLTest.hs
{-# LANGUAGE TemplateHaskell , QuasiQuotes #-}
import HTML

doc = [html|<html>Hello, <strong>TH</strong> world!</html>|]

main = print . markdown $ doc

markdown [html|<strong>|] = "**" ++ concatMap markdown children ++ "**"
markdown [html|<_>|]      = concatMap markdown children
markdown [html|#text|]    = text
