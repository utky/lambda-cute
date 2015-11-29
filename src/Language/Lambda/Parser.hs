module Language.Lambda.Parser where


import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error 
import Language.Lambda.Syntax
import Data.Either (either)


handleError :: (Show a) => Either ParseError a -> String
handleError (Left e) = foldMap showMessage (errorMessages e)
handleError (Right v) = show v

showMessage :: Message -> String
showMessage (SysUnExpect m) = m ++ "\n"
showMessage (UnExpect m)    = m ++ "\n"
showMessage (Expect m)      = m ++ "\n"
showMessage (Message m)     = m ++ "\n"

runLambdaParser :: String -> Either ParseError Term
runLambdaParser = parse parser "terminal" 

parser :: Parser Term
parser = term

term :: Parser Term
term = var <|> abs' <|> app

symbol :: Parser String
symbol = many alphaNum

var :: Parser Term
var = Var <$> symbol

abs' :: Parser Term
abs' = Abs <$> symbol <*> term

app :: Parser Term
app = App <$> abs' <*> term

lambda :: Parser Char
lambda = char 'λ'

dot :: Parser Char
dot = char '.'

space :: Parser Char
space = char ' '

