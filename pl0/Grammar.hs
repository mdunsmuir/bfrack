module Grammar where

import Parser
import Control.Monad
import Data.List

type Oper = String

data PL0 = Ident String
         | Number Int
         | Term PL0 (Maybe (Oper, PL0))
         | Expression (Maybe Oper) PL0 (Maybe (Oper, PL0))

         | OddConditional PL0
         | CompConditional PL0 Oper PL0

         | AssignStatement { ident :: PL0, expr :: PL0 }
         | CallStatement { ident :: PL0 }
         | InputStatement { ident :: PL0 }
         | OutputStatement { expr :: PL0 }
         | BeginStatement { statement :: PL0, next :: PL0 } | BeginEnd
         | IfStatement { condition :: PL0, statement :: PL0 }
         | WhileStatement { condition :: PL0, statement :: PL0 }

         | ConstAssign { ident :: PL0, num :: PL0, next :: PL0 } | ConstEnd
         | VarDefine { ident :: PL0, next :: PL0 } | VarEnd

         | Procedure { ident :: PL0, block :: PL0 }
         | Block { const :: Maybe PL0, var :: Maybe PL0, 
                   procedures :: [PL0], statement :: PL0 }

         deriving Show

parseProgram = do
  prog <- parseBlock
  chompSpaces
  equals "."
  return prog

{-
  basic stuff, expressions
-}

parseIdent = do
  ident <- letters
  return $ Ident ident

parseNumber = do
  numStr <- numbers
  return $ Number $ read numStr

parseFactor = parseIdent <|> parseNumber <|> parseParenExpr
  where parseParenExpr = do
          equals "("
          chompSpaces
          expr <- parseExpression
          chompSpaces
          equals ")"
          return expr

parseTerm = do
  f1 <- parseFactor
  oper <- try $ chompSpaces >> equals "*" <|> equals "/"
  case oper of
    Just oper' -> do nextTerm <- chompSpaces >> parseTerm
                     return $ Term f1 $ Just (oper', nextTerm)
    Nothing ->    return $ Term f1 Nothing

parseExpression = do
  o1 <- try $ equals "-"
  t1 <- parseTerm
  o2 <- try $ chompSpaces >> equals "+" <|> equals "-"
  case o2 of
    Just o2' -> do nextExpression <- chompSpaces >> parseExpression
                   return $ Expression o1 t1 $ Just (o2', nextExpression)
    Nothing  -> return $ Expression o1 t1 Nothing

{-
  conditionals
-}

parseConditional = parseOddConditional <|> parseCompConditional

parseOddConditional = do
  equals "ODD"
  spaces
  expr <- parseExpression
  return $ OddConditional expr

parseCompConditional = do
  e1 <- parseExpression
  chompSpaces
  oper <- equals "#" <|> equals "<=" <|> equals "<" <|>
            equals "=" <|> equals ">=" <|> equals ">"
  chompSpaces
  e2 <- parseExpression
  return $ CompConditional e1 oper e2

{-
  statements
-}

parseStatement = parseAssignStatement <|> parseCallStatement <|> parseInputStatement <|>
                   parseOutputStatement <|> parseBeginStatement <|>
                   parseIfStatement <|> parseWhileStatement

parseAssignStatement = do
  ident <- parseIdent
  spaces
  equals ":="
  spaces
  expr <- parseExpression
  return $ AssignStatement ident expr

parseCallStatement = do
  equals "CALL"
  spaces
  ident <- parseIdent
  return $ CallStatement ident

parseInputStatement = do
  equals "?"
  spaces
  ident <- parseIdent
  return $ InputStatement ident

parseOutputStatement = do
  equals "!"
  spaces
  expr <- parseExpression
  return $ OutputStatement $ expr

parseBeginStatement = do
  equals "BEGIN"
  spaces
  parseStatements
  where parseStatements = do
          stmt <- parseStatement
          semicol <- maybeParse $ chompSpaces >> equals ";"
          case semicol of
            Just _ -> do chompSpaces
                         next <- parseStatements
                         return $ BeginStatement stmt next
            Nothing -> spaces >> equals "END" >> (return $ BeginStatement stmt BeginEnd)

parseIfStatement = do
  equals "IF"
  spaces
  cond <- parseConditional
  spaces
  equals "THEN"
  spaces
  stmt <- parseStatement
  return $ IfStatement cond stmt

parseWhileStatement = do  
  equals "WHILE"
  spaces
  cond <- parseConditional
  spaces
  equals "DO"
  spaces
  stmt <- parseStatement
  return $ WhileStatement cond stmt

{-
  consts, vars
-}

parseConstAssignment = do
  ident <- parseIdent
  chompSpaces
  equals "="
  chompSpaces 
  num <- parseNumber
  return (ident, num) 

parseConstAssignList = do
  equals "CONST"
  spaces
  parseList <|> parseFinal

  where parseList = do
          (ident, num) <- parseConstAssignment
          chompSpaces
          equals ","
          chompSpaces
          next <- parseList <|> parseFinal
          return $ ConstAssign ident num next

        parseFinal = do
          (ident, num) <- parseConstAssignment
          chompSpaces
          equals ";"
          return $ ConstAssign ident num ConstEnd

parseVarDefineList = do
  equals "VAR"
  spaces
  parseList <|> parseFinal

  where parseList = do
          ident <- parseIdent
          chompSpaces
          equals ","
          chompSpaces
          next <- parseList <|> parseFinal
          return $ VarDefine ident next
        
        parseFinal = do
          ident <- parseIdent
          chompSpaces
          equals ";"
          return $ VarDefine ident VarEnd

{-
  procedures and blocks
-}

parseProcedure = do
  equals "PROCEDURE"
  spaces
  ident <- parseIdent
  chompSpaces
  equals ";"
  chompSpaces
  content <- parseBlock
  chompSpaces
  equals ";"
  return $ Procedure ident content 

parseBlock = do
  const <- maybeParse parseConstAssignList
  chompSpaces
  var <- maybeParse parseVarDefineList
  chompSpaces
  procedures <- parseProcedures
  chompSpaces
  stmt <- parseStatement
  return $ Block const var procedures stmt

  where parseProcedures = do
          proc <- maybeParse parseProcedure
          chompSpaces
          case proc of
            Just proc -> liftM2 (:) (return proc) parseProcedures
            Nothing -> return []
