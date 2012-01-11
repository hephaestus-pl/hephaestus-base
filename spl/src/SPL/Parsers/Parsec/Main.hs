module Main where 

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple  = letter

word    :: Parser String
word    = many1 letter

-- 
-- A simple parser for feature expressions.
-- 
parseFeatureExpression 
   = do { try (string "And");
          char '(';
          l <- parseFeatureExpression;
          char ',';
          r <- parseFeatureExpression;
          char ')';
          return (l ++ " and " ++ r)
        }   
  <|> do { try (string "Or");
          char '(';
          l <- parseFeatureExpression;
          char ',';
          r <- parseFeatureExpression;
          char ')';
          return (l ++ " or " ++ r)
        }       
 <|> do { try (string "Not");
          char '(';
          l <- parseFeatureExpression;
          char ')';
          return (" not " ++ l)
        }               
  <|> word
  <|> return "erro"

run :: Show a => Parser a -> String -> IO ()
run p input = 
    case (parse p "" input) of
     Left err -> do { putStr "parse error at "
                      ; print err
                    }
     Right x  -> print x
