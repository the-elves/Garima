import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "+-*/%"


data IntExpr = Number Integer
               |IntVar String
               |IntBinaryExpression Char IntExpr IntExpr

data BoolExpr = BoolLiteral Bool
               |BoolVar String
               |BoolBinaryExpression String IntExpr IntExpr
                
data Expr = IntExpr
            |BoolExpr

data AssignStmt = BoolAssignExpr String BoolExpr
                |IntAssignLiteral String IntExpr
                

data Token = DatatypeKeyword String
             
showToken :: Token -> String
showToken (Number a) = show a

instance Show Token where show = showToken

parseIdentifier::Parser String
parseIdentifier = do
  first <- letter
  rest <- many (letter  <|> digit)
  let a = first:rest
  return a

parseBoolLit::Parser BoolExpr
parseBoolLit = do
   a <- string "false" <|> string "true"
   return $ case a of
     "true" -> BoolLiteral True
     "false" -> BoolLiteral False

parseDatatype::Parser Token
parseDatatype = do
  a <- string "int" 
       <|> string "string"<|>
        string "bool"
  return $ DatatypeKeyword a


------------------------------------------tokens--------------------------------------------
spaces :: Parser()
spaces = skipMany1 space

parseNumber:: Parser Integer
parseNumber = liftM (Number . read ) $ many1(digit)

parseStringLiteral :: Parser String
parseStringLiteral = do
  char '"'
  a<- many letter<|>digit<|>symbol
  char '"'
  return $ a
                 

program::Parser()
program = do
  string "module"
  spaces
  parseIdentifier
  spaces
  string ":"
  newline
  many parseVarDeclaration
  parseSignalDeclarationBlock
  many parseEntityDeclaration
  string "Effects:"
  parseEffectDeclarationBlock
  return ()
-------------------------Larger parsing structures. ---------------------------------
parseIdList::Parser [Token]
parseIdList = parseIdentifier `sepBy` (char ',')

parseVarDeclaration::Parser (Token, [Token])
parseVarDeclaration = do
  typedecl <- parseDatatype
  spaces
  list <- parseIdList
  newline
  return (typedecl,list)
  
parseEntityDeclaration::Parser (String, [Token])
parseEntityDeclaration = do
  string "entity"
  spaces
  list <- parseIdList
  newline
  return (show "entity",list)


parseSingleSignalDeclaration :: Parser (String, String, String,String )
parseSingleSignalDeclaration = do
  string "signal"
  spaces
  sType <- parseDatatype
  char '|'
  sName <- parseIdentifier
  char '|'
  sExe <- parseIdentifier
  char '|'
  sHandler <- parseIdentifier
  newline
  return (sType, sName, sExe,sHandler)

parseSignalDeclarationBlock::Parser [(String, String,String,String)]
parseSignalDeclarationBlock = do
  many parseSingleSignalDeclaration


parseSingleEffectDeclaration :: Parser (String, String, String)
parseSingleEffectDeclaration = do
  string "effect"
  spaces
  eName <- parseIdentifier
  char '|'
  eExe <- parseIdentifier
  string "on"
  sEntity <- parseIdentifier
  newline
  return (eName, eExe,sEntity)

parseEffectDeclarationBlock::Parser [(String,String,String)]
parseEffectDeclarationBlock = do
  many parseSingleEffectDeclaration


----------------------------------------------------Statements---------------------------------------------------------------------------------------

parseLogicalOperation:: Parser BoolExpr
parseLogicalOperation = do
  a<-parseIdentifier
  b<-parseLogicalOp
  c<-parseIdentifier
  return $ BoolBinaryExpression b a c

parseIntExp :: Parser IntExpr
parseIntExp = do
  a <- parseIdentifier
  b <-  parseBinOp
  c <- parseIdentifier
  return $ IntBinaryExpression b a c


parseBinOp :: Parser Char
parseBinOp = do
  a<-char '+'
      <|>char '+'
      <|>char '-'
      <|>char '*'
      <|>char '/'
      <|>char '%'
  return a

parseLogicalOp :: Parser String
parseLogicalOp = do
  a<-string "&&"
      <|>string "||"
  return a

parseExpr :: Parser Expr
parseExpr = do
  try parseIntExp <|> parse

assignExpr :: Parser AssignStmt
assignExpr = do
  a<- parseIdentifier
  b<- char '='
  c<- try parseIntExp
    <|> parseLogicalOperation
  return $ case c  of
    IntIdentifier -> Expr1 a c
    IntExpression ->IntAssignExpr a c
    LogicalExpression -> BoolAssignExpr a c
    LogicalIdentifier -> BoolAssignExpr a c




----------------------------------vvvvv------Managemnet Function vvvv---------------------------------------------------------------------------------



readVarDecl :: String -> String
readVarDecl input = case parse parseVarDeclaration "var" input of
  Left err -> "ERR" ++ show err
  Right (a,b) -> (show a) ++ (show b)
  
readProgram :: String -> String
readProgram input = case parse program "prog" input of
  Left err -> "ERR" ++ show err
  Right _ -> "Program Parsed"
                      

readExpr :: String -> String
readExpr input = case parse parseToken "Garima" input of
  Left err -> "No match" ++ show err
  Right val -> case val of Identifier a -> "Identifier " ++ a
                           Number n -> "Number " ++ show n
                           DatatypeKeyword a -> "Datatype" ++ a
                           ModuleKeyword modulem -> "module"
                           SignalKeyword s -> "Signal"
                           Bool s -> "Bool" ++ show s


main :: IO()
main = do
  (expr:_)<-getArgs
  putStrLn(readVarDecl expr)


