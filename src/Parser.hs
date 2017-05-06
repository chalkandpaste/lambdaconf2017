module Parser where

import Untyped

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String (Parser)

variable = many letterChar
space' = some spaceChar
parenthesized = between (char '(' <* space) (space *> char ')')

expressionParser :: Parser Expression
expressionParser = try letBlock <|> expressionParser' where
  letBlock = do
    var <- string "let" *> space *> variable <* space <* string "=" <* space
    expr1 <- expressionParser' <* space <* string "in" <* space
    expr2 <- expressionParser
    return $ Let var expr1 expr2

expressionParser' :: Parser Expression
expressionParser' = makeExprParser term table where

  term = try (parenthesized expressionParser')
    <|> try (Literal <$> valueParser)
    <|> (Variable <$> variable)

  table =
    [ [ Prefix (UnOp <$> unOpParser) ]
    , [ InfixR (BinOp <$> binOpParser And "and")
      , InfixR (BinOp <$> binOpParser Or "or")
      , InfixR (BinOp <$> binOpParser Equ "==")
      , InfixR (BinOp <$> binOpParser Neq "!=")
      , InfixR (BinOp <$> binOpParser Gt ">")
      , InfixR (BinOp <$> binOpParser Gte ">=")
      , InfixR (BinOp <$> binOpParser Lt "<")
      , InfixR (BinOp <$> binOpParser Lte "<=")
      , InfixR (BinOp <$> binOpParser Add "+")
      , InfixR (BinOp <$> binOpParser Sub "-")
      , InfixR (BinOp <$> binOpParser Mul "*")
      ]
    ]

  unOpParser = try (Not <$ string "not" <* space')
    <|> (Abs <$ string "abs" <* space')
  binOpParser :: BinOp -> String -> Parser BinOp
  binOpParser op str = try $ op <$ (space' *> string str <* space')

valueParser :: Parser Value
valueParser = try bool <|> try double <|> int
  where
    bool = fmap VBool $
      try (True <$ string "true")
      <|> (False <$ string "false")
    double = VDouble <$> L.signed space L.float
    int = VInt . fromIntegral <$> L.signed space L.integer
