{-# LANGUAGE DeriveDataTypeable #-}

module Interpreter (run) where

import Control.Monad.State.Lazy
import Control.Exception
import Data.Typeable
import Data.List
import qualified Data.Map.Lazy as M
import Grammar hiding (procedures)

data IdentNotFoundException = IdentNotFoundException String deriving (Show, Typeable)
instance Exception IdentNotFoundException

data BlockScope = BlockScope { consts :: M.Map String Int,
                               vars :: M.Map String Int,
                               procedures :: M.Map String PL0 }

type Interpreter = StateT [BlockScope] IO

{-
  function to search the scope stack for an ident
-}

scopeLookup :: (BlockScope -> M.Map String a) -> String -> Interpreter (Maybe a)
scopeLookup context name = do
  scope <- get
  return $ scopeLookup' context name scope

scopeLookup' :: (BlockScope -> M.Map String a) -> String -> [BlockScope] -> Maybe a
scopeLookup' _ name [] = Nothing
scopeLookup' context name (s:ss) = let map = context s
                                   in  case M.lookup name map of
                                         Just x -> Just x
                                         Nothing -> scopeLookup' context name ss

{-
  expression evaluation
-}

opers = [
  ("+", (+)),
  ("-", flip subtract),
  ("*", (*)),
  ("/", div) ]

eval :: PL0 -> Interpreter Int
eval (Number x) = return x

eval (Ident name) = do
  constResult <- scopeLookup consts name
  case constResult of
    Just x -> return x
    Nothing -> do varResult <- scopeLookup vars name
                  case varResult of
                    Just x -> return x
                    Nothing -> liftIO $ throwIO $ IdentNotFoundException name
    

eval (Term left (Just (oper, right))) = liftM2 oper' (eval left) (eval right)
  where (Just oper') = lookup oper opers

eval (Term left Nothing) = eval left

eval (Expression o1 t1 t2) = 
  case t2 of
    Just (o2, t2') -> let o2' = case lookup o2 opers of
                                  Just o -> o
                      in  liftM2 o2' evalT1 $ eval t2'
    Nothing -> evalT1
  where evalT1 = liftM negate' $ eval t1
        negate' = case o1 of
                   Just "-" -> negate
                   Nothing -> id

{-
  conditional evaluation
-}

condOpers = [("=", (==)), ("#", (/=)),
             ("<", (<)), ("<=", (<=)),
             (">", (>)), (">=", (>=))]

evalCond :: PL0 -> Interpreter Bool

evalCond (OddConditional expr) = do
  exprVal <- eval expr
  return $ odd exprVal

evalCond (CompConditional left oper right) = do
  leftVal <- eval left
  rightVal <- eval right
  let oper' = lookup oper condOpers
  case oper' of Just o -> return $ o leftVal rightVal

{-
  const, variable assignment
-}

assign (AssignStatement (Ident name) _) _ [] = throw $ IdentNotFoundException name
assign as@(AssignStatement (Ident name) expr) val (s:ss)
  | varDefined = (BlockScope constMap varMap' procMap) : ss
  | otherwise  = s : assign as val ss
  where 
    (BlockScope constMap varMap procMap) = s
    varDefined = M.member name varMap
    varMap' = M.insert name val varMap

execute :: PL0 -> Interpreter ()

-- AssignStatement
execute as@(AssignStatement _ expr) = do
  val <- eval expr
  scope <- get
  put $ assign as val scope

-- ConstAssign
execute ConstEnd = return ()
execute (ConstAssign (Ident name) (Number val) next) = do
  ((BlockScope constMap varMap procMap):ss) <- get
  let constMap' = M.insert name val constMap
  put $ (BlockScope constMap' varMap procMap) : ss
  execute next

-- VarDefine
execute VarEnd = return ()
execute (VarDefine (Ident name) next) = do
  ((BlockScope constMap varMap procMap):ss) <- get
  let varMap' = M.insert name 0 varMap
  put $ (BlockScope constMap varMap' procMap) : ss
  execute next

{-
  statements
-}

-- CallStatement
execute (CallStatement (Ident name)) = do
  block <- scopeLookup procedures name
  case block of
    Just b -> execute b
    Nothing -> liftIO $ throwIO $ IdentNotFoundException name

-- InputStatement
execute (InputStatement name) = do
  val <- liftIO (readLn :: IO Int)
  execute $ AssignStatement name (Number val)

-- OutputStatement
execute (OutputStatement expr) = do
  val <- eval expr
  liftIO $ putStrLn $ show val

-- BeginEnd
execute BeginEnd = return ()
execute (BeginStatement statement next) = execute statement >> execute next

-- IfStatement
execute (IfStatement cond statement) = do
  condVal <- evalCond cond
  if condVal then execute statement else return ()

-- WhileStatement
execute ws@(WhileStatement cond statement) = do
  condVal <- evalCond cond
  if condVal then execute statement >> execute ws else return ()

{-
  block
-}

execute (Block const var procedures statement) = do
  let procedures' = map (\(Procedure (Ident name) block) -> (name, block)) procedures
  scope <- get 
  put $ (BlockScope M.empty M.empty (M.fromList procedures')) : scope

  maybeDo const
  maybeDo var

  execute statement

  -- pop my new scope off the stack, we don't want to keep it around
  (_:scope') <- get
  put scope' 
  where 
    maybeDo assignments = case assignments of
                            Just a -> execute a
                            Nothing -> return ()

{-
  main function to run it
-}

run :: PL0 -> IO ()
run pl0 = runStateT (execute pl0) [] >> return ()
