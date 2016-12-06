import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "+-*/%"


data Token =  Identifier String 
             |Number Integer
             |Bool Bool
             |DatatypeKeyword String
             |ModuleKeyword String
             |SignalKeyword String
             |EntityKeyword String
             |EffectKeyword String
showToken :: Token -> String
showToken (Identifier a) = show a
showToken (Bool a) = show a
showToken (Number a) = show a
showToken (DatatypeKeyword a) = "Token type"
showToken (ModuleKeyword a) = "ModuleToken"
showToken (SignalKeyword a) = "SignalToken"
showToken (EntityKeyword a) = "Entity"
showToken (EffectKeyword a) = "Effect"

instance Show Token where show = showToken

parseIdentifier::Parser Token
parseIdentifier = do
  first <- letter
  rest <- many (letter  <|> digit)
  let a = first:rest
  return $ Identifier a

parseBoolLit::Parser Token
parseBoolLit = do
   a <- string "false" <|> string "true"
   return $ case a of
     "true" -> Bool True
     "false" -> Bool False

parseDatatype::Parser Token
parseDatatype = do
  a <- string "int" 
       <|> string "string"<|>
        string "bool"
  return $  DatatypeKeyword a

parseModuleKeyword::Parser Token
parseModuleKeyword = do
  a<-string "module"
  return $ ModuleKeyword a
  
parseSignalKeyword::Parser Token
parseSignalKeyword = do
  a<-string "signal"
  return $ SignalKeyword a

parseEntityKeyword::Parser Token
parseEntityKeyword = do
  a<-string "entity"
  return $ EntityKeyword a

parseEffectKeyword::Parser Token
parseEffectKeyword = do
  a<-string "effect"
  return $ EntityKeyword a


parseKeyword :: Parser Token
parseKeyword = try parseDatatype
               <|>try parseEntityKeyword
               <|>try parseEffectKeyword
               <|>try parseModuleKeyword
               <|>try parseSignalKeyword
               <|>parseBoolLit
------------------------------------------tokens--------------------------------------------
spaces :: Parser()
spaces = skipMany1 space

parseNumber:: Parser Token
parseNumber = liftM ( Number . read ) $ many1(digit)

parseToken::Parser Token
parseToken = parseKeyword
  <|>parseIdentifier
  <|>parseNumber
  

program::Parser()
program = do
  string "module"
  spaces
  parseIdentifier
  spaces
  string ":"
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
  entity <- parseEntityKeyword
  spaces
  list <- parseIdList
  newline
  return (show entity,list)


parseSingleSignalDeclaration :: Parser (Token,Token, Token, Token)
parseSingleSignalDeclaration = do
  parseSignalKeyword
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

parseSignalDeclarationBlock::Parser [(Token, Token,Token,Token)]
parseSignalDeclarationBlock = do
  many parseSingleSignalDeclaration


parseSingleEffectDeclaration :: Parser (Token, Token, Token)
parseSingleEffectDeclaration = do
  parseEffectKeyword
  spaces
  eName <- parseIdentifier
  char '|'
  eExe <- parseIdentifier
  string "on"
  sEntity <- parseIdentifier
  newline
  return (eName, eExe,sEntity)

parseEffectDeclarationBlock::Parser [(Token,Token,Token)]
parseEffectDeclarationBlock = do
  many parseSingleEffectDeclaration


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


