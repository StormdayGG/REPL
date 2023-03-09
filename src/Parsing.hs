
module Parsing where

import Exp
import Lab2
import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)

parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

var :: Parser Var
var = Var <$> (identifier anyalpha anyalphanum <|> identifier anysymbol anysymbol)
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Parser ComplexExp
varExp = CX <$> var
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

lambdaExp :: Parser ComplexExp
lambdaExp = 
	do
		symbol "\\"
		a <- var
		symbol "->"
		b <- expr
		return (CLam a b)
		
-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

letExp :: Parser ComplexExp
letExp =
	do
		symbol "let"
		a <- var
		symbol ":="
		b <- expr
		symbol "in"
		c <- expr
		return (Let a b c)
-- >>> parseFirst letExp "let x := y in z"
-- Just (Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

letrecExp :: Parser ComplexExp
letrecExp = 
	do
		symbol "letrec"
		a <- var
		symbol ":="
		b <- expr
		symbol "in"
		c <- expr
		return (LetRec a b c)
-- >>> parseFirst letrecExp "letrec x := y in z"
-- Just (LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"})))

listExp :: Parser ComplexExp
listExp =
	do
		a <- brackets (commaSep varExp)
		return (List a)
-- >>> parseFirst listExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

natExp :: Parser ComplexExp
natExp = Nat . fromIntegral <$> natural
-- >>> parseFirst natExp "223 a"
-- Just (Nat 223)

parenExp :: Parser ComplexExp
parenExp =
	do
		a <- parens expr
		return a
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Parser ComplexExp
basicExp = letrecExp <|> letExp  <|> lambdaExp <|> listExp <|> parenExp <|> natExp <|> varExp
-- >>> parseFirst basicExp "[a,b,c]"
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

expr :: Parser ComplexExp
expr = foldl1 CApp <$> some basicExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))

