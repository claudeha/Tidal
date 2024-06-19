module Parsec.Compat where
import Text.ParserCombinators.Parsec
instance Eq ParseError where a == b = False -- FIXME
