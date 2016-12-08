
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Text.Parsec.String
import System.Exit
import System.IO
import System.Cmd


symbol :: Parser Char
symbol = oneOf "+-*/%"

data Var = Var Token [String]
data Signal =  Signal Token String String String
data Condition =  Condition String [String] [String]
data Entity = Entity [String]
data Effect = Effect String String String
data Handler =  Handler String [Stmts]
data Situation =  Situation BoolExpr String String

showVar (Var a b) = show a ++ " "++show b
showSig (Signal a b c d) =  b ++" "++ c
showEffect (Effect a b c) =  a ++ " " ++  b
showSituation (Situation a b c) = b ++ " " ++ c
data Program = Program (String, [Var], [Signal], [Condition], [Entity], [Effect],[Handler], [Situation])
              
data IntExpr = Number Integer
               |IntVar String
               |IntBinaryExpression Char IntExpr IntExpr

data BoolExpr = BoolLiteral Bool
               |BoolVar String
               |BoolBinaryExpression String BoolExpr BoolExpr

data Stmts = AssignStmts AssignStmt
             |RaiseStmts 

data AssignStmt = BoolAssignStmt String BoolExpr
                |IntAssignStmt String IntExpr
                

data Token = DatatypeKeyword String

            
showToken :: Token -> String
showToken (DatatypeKeyword a) = a

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
  a <-  try$ string "int" 
       <|> string "string"<|>
        string "bool"
  return $ DatatypeKeyword a


------------------------------------------tokens--------------------------------------------
spaces :: Parser()
spaces = skipMany1 space

parseNumber:: Parser Integer
parseNumber = liftM ( read ) $ many1(digit)

parseStringLiteral :: Parser String
parseStringLiteral = do
  char '"'
  a <- many $ letter<|>digit<|>symbol
  char '"'
  return $ a
                 

program::Parser Program
program = do
  string "module"
  spaces
  moduleName <- parseIdentifier
  spaces
  string ":"
  newline
  vars <-  many parseVarDeclaration
  signals<- parseSignalDeclarationBlock
  entities <- many parseEntityDeclaration
  string "Effects:"
  newline
  effects <- parseEffectDeclarationBlock
  string "Handlers:"
  newline
  handlers <- parseHandlersBlock
  string "Conditions:"
  newline
  conditions <- try parseConditionsBlock
  string "Situations:"
  newline
  situations <- parseSituationsBlock
  newline
  eof
  -- Program (String, [Var], [Signal], [Condition], [Entity], [Effect],[Handler], [Situation])
  return $ Program ( moduleName, vars, signals, conditions, entities, effects,
             handlers, situations)


-------------------------Larger parsing structures. ---------------------------------

parseIdList::Parser [String ]
parseIdList = parseIdentifier `sepBy` (char ',')

parseVarDeclaration::Parser Var
parseVarDeclaration = do
  typedecl <- parseDatatype
  spaces
  list <- parseIdList
  newline
  return $ Var typedecl list
  
parseEntityDeclaration::Parser Entity
parseEntityDeclaration = do
  string "entity"
  spaces
  list <- parseIdList
  newline
  return $ Entity list


parseSingleSignalDeclaration :: Parser Signal
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
  return $ Signal sType sName sExe sHandler

parseSignalDeclarationBlock::Parser [Signal]
parseSignalDeclarationBlock = do
  a <-  many parseSingleSignalDeclaration
  return a


parseSingleEffectDeclaration :: Parser Effect
parseSingleEffectDeclaration = do
  string "effect"
  spaces
  eName <- parseIdentifier
  char '|'
  eExe <- parseIdentifier
  spaces
  string "on"
  spaces
  sEntity <- parseIdentifier
  newline
  return $ Effect  eName eExe sEntity

parseEffectDeclarationBlock::Parser [Effect]
parseEffectDeclarationBlock = do
  a <- many parseSingleEffectDeclaration
  return a

  

parseSingleSituation :: Parser Situation
parseSingleSituation = do
  string "when"
  spaces
  a<-parseBoolExp
  spaces 
  action<-parseIdentifier
  char '@'
  entity<-parseIdentifier
  newline
  return $ Situation a action entity

parseSituationsBlock :: Parser [Situation]
parseSituationsBlock =do
   many parseSingleSituation >>= return


parseHandlersBlock :: Parser [Handler]
parseHandlersBlock = many parseHandler

--(String, [Stmts])
parseHandler :: Parser Handler
parseHandler = do
  string "handle"
  spaces
  a<-parseIdentifier
  spaces
  string "do"
  newline
  stmts <- many1 parseStmt
  string "done"
  return $ Handler a stmts

parseSingleCondition::Parser Condition
parseSingleCondition = do
  string "condition"
  a<-parseIdentifier
  spaces
  b<-parseArgList
  string ":"
  rhs<- many1 parseIdentifier
  return $ Condition a b rhs



parseConditionsBlock::Parser [Condition]
parseConditionsBlock=many parseSingleCondition



----------------------------------------------------Statements---------------------------------------------------------------------------------------

parseBoolExp:: Parser BoolExpr
parseBoolExp = do
  a<-parseIdentifier
  b<-parseLogicalOp
  c<-parseIdentifier
  let aa = BoolVar a
      cc = BoolVar c
      in
      return $ BoolBinaryExpression b aa cc

parseIntExp :: Parser IntExpr
parseIntExp = do
  a <- parseIntExp
  b <- parseBinOp
  c <- parseIntExp
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
      <|>string "="
  return a




parseStmt :: Parser Stmts
parseStmt = do
  a<-parseAssignStmt
  return $ AssignStmts a


parseIntAssignStmt :: Parser AssignStmt
parseIntAssignStmt = do
  alhs<-parseIdentifier
  char '='
  rhs <- parseIntExp
  return $ IntAssignStmt alhs rhs

parseBoolAssignStmt :: Parser AssignStmt
parseBoolAssignStmt = do
  blhs<-parseIdentifier
  char '='
  brhs <- parseBoolExp
  return $ BoolAssignStmt blhs brhs


parseAssignStmt :: Parser AssignStmt
parseAssignStmt = do
  a<- parseBoolAssignStmt
      <|>parseIntAssignStmt
  return a


           
  
parseArgList :: Parser [String]
parseArgList = do
  char '('
  args<- parseIdentifier `sepBy` (char ',') 
  char ')'
  return args
  


----------------------------------vvvvv------Managemnet Function vvvv---------------------------------------------------------------------------------



--readVarDecl :: String -> String
--readVarDecl input = case parse parseVarDeclaration "var" input of
--  Left err -> "ERR" ++ show err
--  Right (a,b) -> (show a) ++ (show b)
  
readProgram :: String -> String
readProgram input = case parse program "prog" input of
  Left err -> "ERR" ++ show err
  Right _ -> "Program Parsed"
                      

-- readExpr :: String -> String
-- readExpr input = case parse parseToken "Garima" input of
--   Left err -> "No match" ++ show err
--   Right val -> case val of Number n -> "Number " ++ show n
--                            DatatypeKeyword a -> "Datatype" ++ a


parse1 :: Parser a -> String -> IO a
parse1 p fileName = parseFromFile p fileName >>= either report return
  where
    report err = do
        putStrLn $ "Error: " ++ show err
        exitFailure
-- Program (String, [Var], [Signal], [Condition], [Entity], [Effect],[Handler], [Situation])

main :: IO()
main = do
  Program (a,b,c,d,e,f,g,h) <- parse1 program "file1.txt"
  handle <- openFile "config.txt" WriteMode
  hPutStrLn handle ( show $ map showSig c)
  hPutStrLn handle ( show $ map showEffect f)
  hPutStrLn handle ( show $ map showSituation h )
  hClose handle
  io <- system "java Main"
  putStrLn $ show io
  return ()
