{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Dblang.Typecheck (
  Typecheck,
  runTypecheck,
  Error (..),
  checkDefinition,
  checkExpr,
  checkCommand,
  unknown,
  zonk,
  zonkExpr,
) where

import Bound (fromScope, toScope)
import Bound.Scope (transverseScope)
import Bound.Var (unvar)
import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Strict (runWriterT, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable (foldl', for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Dblang.Command (Command (..))
import Dblang.Definition (Definition)
import qualified Dblang.Definition as Definition
import Dblang.Definition.Constraint (Constraint (..))
import Dblang.Definition.Table (Table (..))
import qualified Dblang.Definition.Table as Table
import Dblang.Expr (Expr (..))
import qualified Dblang.Expr as Expr
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type

data Error
  = NotInScope {name :: Text}
  | TypeMismatch {expected :: Type, actual :: Type}
  | Occurs {meta :: Int, type_ :: Type}
  | UnknownConstraint {name :: Text}
  | NotAField {expr :: Syntax.Expr Void, table :: Text}
  | IncorrectConstraintArguments {expectedArguments :: Int, actualArguments :: [Syntax.Expr Void]}
  | NotATable {table :: Text}
  | IncorrectConstructorArguments {expectedArgumentCount :: Int, actualArgumentCount :: Int}
  deriving (Eq, Show)

newtype Typecheck a = Typecheck (StateT (HashMap Int (Maybe Type)) (Either Error) a)
  deriving (Functor, Applicative, Monad, MonadError Error)

runTypecheck :: Typecheck a -> Either Error a
runTypecheck (Typecheck ma) = evalStateT ma mempty

unknown :: Typecheck Type
unknown =
  Typecheck $ do
    unknowns <- get
    let next = HashMap.size unknowns
    put $ HashMap.insert next Nothing unknowns
    pure $ Type.Unknown next

getSolution :: Int -> Typecheck (Maybe Type)
getSolution n =
  Typecheck $ do
    mSolution <- gets (HashMap.lookup n)
    maybe undefined pure mSolution

setSolution :: Int -> Type -> Typecheck ()
setSolution n ty =
  Typecheck . modify $ HashMap.insert n (Just ty)

zonk :: Type -> Typecheck Type
zonk ty =
  case ty of
    Type.Unknown n -> do
      solution <- getSolution n
      case solution of
        Nothing ->
          pure ty
        Just ty' ->
          zonk ty'
    Type.App a b ->
      Type.App <$> zonk a <*> zonk b
    Type.RCons a b c ->
      Type.RCons a <$> zonk b <*> zonk c
    Type.Name{} ->
      pure ty
    Type.RNil ->
      pure ty

zonkExpr :: Expr a -> Typecheck (Expr a)
zonkExpr expr =
  case expr of
    Expr.Var{} ->
      pure expr
    Expr.Name{} ->
      pure expr
    Expr.Ctor name args ->
      Expr.Ctor name <$> traverse zonkExpr args
    Expr.Int{} ->
      pure expr
    Expr.Bool{} ->
      pure expr
    Expr.String{} ->
      pure expr
    Expr.Lam name body ->
      Expr.Lam name <$> transverseScope zonkExpr body
    Expr.App ty a b ->
      Expr.App <$> zonk ty <*> zonkExpr a <*> traverse zonkExpr b
    Expr.Yield a ->
      Expr.Yield <$> zonkExpr a
    Expr.GroupBy t1 t2 a b ->
      Expr.GroupBy <$> zonk t1 <*> zonk t2 <*> zonkExpr a <*> zonkExpr b
    Expr.For name ty a b ->
      Expr.For name <$> zonk ty <*> zonkExpr a <*> transverseScope zonkExpr b
    Expr.Where condition rest ->
      Expr.Where <$> zonkExpr condition <*> zonkExpr rest
    Expr.Dot ty a b ->
      Expr.Dot <$> zonk ty <*> zonkExpr a <*> pure b
    Expr.Splat a b ->
      Expr.Splat <$> zonkExpr a <*> pure b
    Expr.Record fields ->
      Expr.Record <$> (traverse . traverse) zonkExpr fields
    Expr.Equals a b ->
      Expr.Equals <$> zonkExpr a <*> zonkExpr b

unify :: Type -> Type -> Typecheck ()
unify expected actual = do
  expected' <- walk expected
  actual' <- walk actual
  unifyWalked expected' actual'
 where
  walk :: Type -> Typecheck Type
  walk ty@(Type.Unknown n) = do
    solution <- getSolution n
    maybe (pure ty) walk solution
  walk ty = pure ty

unifyWalked :: Type -> Type -> Typecheck ()
unifyWalked expected actual =
  case expected of
    Type.Unknown n ->
      case actual of
        Type.Unknown n'
          | n == n' ->
              pure ()
        _ ->
          solveLeft n actual
    Type.Name name ->
      case actual of
        Type.Name name'
          | name == name' ->
              pure ()
        Type.Unknown n ->
          solveRight expected n
        _ ->
          throwError TypeMismatch{expected, actual}
    Type.App expectedA expectedB ->
      case actual of
        Type.App actualA actualB -> do
          unify expectedA actualA
          unify expectedB actualB
        Type.Unknown n ->
          solveRight expected n
        _ ->
          throwError TypeMismatch{expected, actual}
    Type.RCons{} ->
      case actual of
        Type.RCons{} -> do
          let (expectedFields, expectedRemainder) = Type.matchRow expected
          let (actualFields, actualRemainder) = Type.matchRow actual

          let (common, expectedNotInActualFields) =
                Vector.partitionWith
                  ( \(field, expectedTy) -> case Vector.find (\(field', _) -> field == field') actualFields of
                      Nothing -> Right (field, expectedTy)
                      Just (_field, actualTy) -> Left (field, expectedTy, actualTy)
                  )
                  expectedFields
              actualNotInExpectedFields =
                Vector.filter
                  ( \(field, _) -> case Vector.find (\(field', _, _) -> field == field') common of
                      Nothing -> True
                      Just _ -> False
                  )
                  actualFields
          for_ common $ \(_field, expectedTy, actualTy) -> unify expectedTy actualTy

          newRemainder <- unknown

          unify
            (Maybe.fromMaybe Type.RNil expectedRemainder)
            (foldr (uncurry Type.RCons) newRemainder actualNotInExpectedFields)

          unify
            (foldr (uncurry Type.RCons) newRemainder expectedNotInActualFields)
            (Maybe.fromMaybe Type.RNil actualRemainder)
        Type.Unknown n ->
          solveRight expected n
        _ ->
          throwError TypeMismatch{expected, actual}
    Type.RNil ->
      case actual of
        Type.RNil ->
          pure ()
        Type.Unknown n ->
          solveRight expected n
        _ ->
          throwError TypeMismatch{expected, actual}
 where
  occurs :: Int -> Type -> Bool
  occurs n ty =
    case ty of
      Type.Unknown n' -> n == n'
      Type.Name{} -> False
      Type.App a b -> occurs n a || occurs n b
      Type.RCons _ a b -> occurs n a || occurs n b
      Type.RNil -> False

  solveLeft :: Int -> Type -> Typecheck ()
  solveLeft n ty = do
    when (occurs n ty) $ throwError Occurs{meta = n, type_ = ty}
    solution <- getSolution n
    case solution of
      Nothing ->
        setSolution n ty
      Just ty' ->
        unify ty' ty

  solveRight :: Type -> Int -> Typecheck ()
  solveRight ty n = do
    when (occurs n ty) $ throwError Occurs{meta = n, type_ = ty}
    solution <- getSolution n
    case solution of
      Nothing ->
        setSolution n ty
      Just ty' ->
        unify ty ty'

checkDefinition :: Syntax.Definition -> Typecheck Definition
checkDefinition definition =
  case definition of
    Syntax.Table{name, items} -> do
      (types, inFields, outFields, constraints) <- checkTableItems name items
      pure $ Definition.Table Table{name, types, inFields, outFields, constraints}

checkTableItems ::
  Text ->
  Vector Syntax.TableItem ->
  Typecheck (Vector (Text, Type), Vector (Text, Type), Vector (Text, Type), Vector Constraint)
checkTableItems table items_ = do
  ((), (types, _fields, inFields, outFields, constraints)) <- flip runStateT (mempty, mempty, mempty, mempty, mempty) $ go items_
  pure
    ( Vector.fromList $ DList.toList types
    , Vector.fromList $ DList.toList inFields
    , Vector.fromList $ DList.toList outFields
    , Vector.fromList $ DList.toList constraints
    )
 where
  go ::
    Vector Syntax.TableItem ->
    StateT
      (DList (Text, Type), HashMap Text Type, DList (Text, Type), DList (Text, Type), DList Constraint)
      Typecheck
      ()
  go items =
    case Vector.uncons items of
      Nothing ->
        pure ()
      Just (item, items') ->
        case item of
          Syntax.Type{name, value} -> do
            modify $ \(types, fields, inFields, outFields, constraints) ->
              ( types `DList.snoc` (name, value)
              , fields
              , inFields
              , outFields
              , constraints
              )
            go items'
          Syntax.Field{name, type_, constraints = fieldConstraints} -> do
            modify $ \(types, fields, inFields, outFields, constraints) ->
              ( types
              , HashMap.insert name type_ fields
              , inFields
              , outFields
              , constraints
              )

            (constraints', Any hasDefault) <- do
              (typeDefinitions, fields) <- gets $ \(types, fields, _inFields, _outFields, _constraints) ->
                ( foldl' (\acc (typeName, typeValue) -> HashMap.insert typeName typeValue acc) mempty types
                , fields
                )
              runWriterT $
                traverse
                  ( \constraint -> do
                      constraint' <-
                        lift . lift $
                          checkConstraint
                            table
                            typeDefinitions
                            fields
                            constraint.name
                            (Syntax.Name name : Vector.toList constraint.arguments)

                      case constraint' of
                        Default{} -> tell $ Any True
                        _ -> pure ()

                      pure constraint'
                  )
                  (Vector.toList fieldConstraints)

            modify $ \(types, fields, inFields, outFields, constraints) ->
              ( types
              , fields
              , inFields `DList.snoc` (name, if hasDefault then Type.App (Type.Name "Optional") type_ else type_)
              , outFields `DList.snoc` (name, type_)
              , constraints <> DList.fromList constraints'
              )

            go items'
          Syntax.Constraint constraint -> do
            constraint' <- do
              (typeDefinitions, fields) <- gets $ \(types, fields, _inFields, _outFields, _constraints) ->
                ( foldl' (\acc (name, value) -> HashMap.insert name value acc) mempty types
                , fields
                )
              lift $ checkConstraint table typeDefinitions fields constraint.name (Vector.toList constraint.arguments)
            modify $ \(types, fields, inFields, outFields, constraints) ->
              ( types
              , fields
              , inFields
              , outFields
              , constraints `DList.snoc` constraint'
              )
            go items'

checkConstraint ::
  Text ->
  HashMap Text Type ->
  HashMap Text Type ->
  Text ->
  [Syntax.Expr Void] ->
  Typecheck Constraint
checkConstraint table typeDefinitions fields name arguments =
  case name of
    "Default" ->
      case arguments of
        [expr, value] ->
          case expr of
            Syntax.Name fieldName
              | Just type_ <- HashMap.lookup fieldName fields -> do
                  value' <- checkExpr mempty absurd value (Type.replaceDefinitions typeDefinitions type_)
                  pure $ Default fieldName value'
            _ -> throwError NotAField{expr, table}
        _ -> throwError IncorrectConstraintArguments{expectedArguments = 2, actualArguments = arguments}
    "Key" -> do
      arguments' <-
        traverse
          ( \argument ->
              case argument of
                Syntax.Name fieldName ->
                  pure fieldName
                _ ->
                  throwError NotAField{expr = argument, table}
          )
          arguments
      pure . Key $ Vector.fromList arguments'
    "PrimaryKey" -> do
      arguments' <-
        traverse
          ( \argument ->
              case argument of
                Syntax.Name fieldName ->
                  pure fieldName
                _ ->
                  throwError NotAField{expr = argument, table}
          )
          arguments
      pure . PrimaryKey $ Vector.fromList arguments'
    _ -> throwError UnknownConstraint{name}

checkExpr :: HashMap Text Type -> (a -> Type) -> Syntax.Expr a -> Type -> Typecheck (Expr a)
checkExpr nameTypes varType expr expectedTy =
  case expr of
    Syntax.Name name ->
      case HashMap.lookup name nameTypes of
        Nothing ->
          throwError $ NotInScope name
        Just actualTy ->
          Name name <$ unify expectedTy actualTy
    Syntax.Var var ->
      Var var <$ unify expectedTy (varType var)
    Syntax.Ctor "None" args -> do
      unless (null args)
        . throwError
        $ IncorrectConstructorArguments{expectedArgumentCount = 0, actualArgumentCount = length args}
      innerTy <- unknown
      unify expectedTy (Type.App (Type.Name "Optional") innerTy)
      pure (Ctor "None" [])
    Syntax.Ctor "Some" args -> do
      case args of
        [arg] -> do
          innerTy <- unknown
          unify expectedTy (Type.App (Type.Name "Optional") innerTy)
          arg' <- checkExpr nameTypes varType arg innerTy
          pure (Ctor "Some" [arg'])
        _ -> throwError $ IncorrectConstructorArguments{expectedArgumentCount = 1, actualArgumentCount = length args}
    Syntax.Ctor name _arg ->
      error $ "TODO: type check Ctor " <> show name
    Syntax.Lam name body -> do
      inTy <- unknown
      outTy <- unknown
      unify expectedTy (Type.arrow inTy outTy)
      body' <- checkExpr nameTypes (unvar (\() -> inTy) varType) (fromScope body) outTy
      pure $ Lam name (toScope body')
    Syntax.App f x -> do
      inTy <- unknown
      let outTy = expectedTy
      f' <- checkExpr nameTypes varType f (Type.arrow inTy outTy)
      x' <- checkExpr nameTypes varType x inTy
      pure $ App outTy f' [x']
    Syntax.Dot expr' field -> do
      remainder <- unknown
      expr'' <- checkExpr nameTypes varType expr' (Type.App (Type.Name "Record") (Type.RCons field expectedTy remainder))
      pure $ Dot expectedTy expr'' field
    Syntax.Splat expr' fields -> do
      fieldTypes <- traverse (\field -> (,) field <$> unknown) fields
      unify expectedTy (Type.record fieldTypes)
      remainder <- unknown
      expr'' <- checkExpr nameTypes varType expr' (Type.App (Type.Name "Record") (foldr (uncurry Type.RCons) remainder fieldTypes))
      pure $ Splat expr'' fields
    Syntax.For name collection rest -> do
      b <- unknown
      unify expectedTy (Type.App (Type.Name "Relation") b)
      a <- unknown
      collection' <- checkExpr nameTypes varType collection (Type.App (Type.Name "Relation") a)
      rest' <- checkExpr nameTypes (unvar (\() -> a) varType) (fromScope rest) (Type.App (Type.Name "Relation") b)
      pure $ For name a collection' (toScope rest')
    Syntax.Where condition rest -> do
      a <- unknown
      unify expectedTy (Type.App (Type.Name "Relation") a)
      condition' <- checkExpr nameTypes varType condition (Type.Name "Bool")
      rest' <- checkExpr nameTypes varType rest (Type.App (Type.Name "Relation") a)
      pure $ Where condition' rest'
    Syntax.Yield value -> do
      a <- unknown
      unify expectedTy (Type.App (Type.Name "Relation") a)
      value' <- checkExpr nameTypes varType value a
      pure $ Yield value'
    Syntax.GroupBy collection projection -> do
      {-
      a : Relation v         b : v -> k
      ---------------------------------
      a group by b : Map k (Relation v)
      -}
      keyTy <- unknown
      valueTy <- unknown
      unify expectedTy (Type.App (Type.App (Type.Name "Map") keyTy) (Type.App (Type.Name "Relation") valueTy))
      collection' <- checkExpr nameTypes varType collection (Type.App (Type.Name "Relation") valueTy)
      projection' <- checkExpr nameTypes varType projection (Type.arrow valueTy keyTy)
      pure $ GroupBy keyTy valueTy collection' projection'
    Syntax.Record fields -> do
      expectedRow <- unknown
      unify expectedTy (Type.App (Type.Name "Record") expectedRow)
      let (expectedFields, expectedRemainder) = Type.matchRow expectedRow
      (fields', newExpectedRemainder) <-
        flip runStateT (Maybe.fromMaybe Type.RNil expectedRemainder) $
          traverse
            ( \(field, value) -> do
                ty <- case Vector.find (\(field', _ty) -> field == field') expectedFields of
                  Nothing -> do
                    ty <- lift unknown
                    newExpectedRemainder <- lift unknown
                    currentExpectedRemainder <- get
                    lift $ unify currentExpectedRemainder (Type.RCons field ty newExpectedRemainder)
                    put newExpectedRemainder
                    pure ty
                  Just (_field, ty) ->
                    pure ty
                lift $ (,) field <$> checkExpr nameTypes varType value ty
            )
            fields
      unify newExpectedRemainder Type.RNil
      pure $ Record fields'
    Syntax.Equals a b -> do
      t <- unknown
      a' <- checkExpr nameTypes varType a t
      b' <- checkExpr nameTypes varType b t
      Equals a' b' <$ unify expectedTy (Type.Name "Bool")
    Syntax.Int i ->
      Int i <$ unify expectedTy (Type.Name "Int")
    Syntax.Bool b ->
      Bool b <$ unify expectedTy (Type.Name "Bool")
    Syntax.String s ->
      String s <$ unify expectedTy (Type.Name "String")

lookupTable :: Vector Definition -> Text -> Maybe Table
lookupTable definitions name =
  Vector.mapMaybe
    (\(Definition.Table table) -> if table.name == name then Just table else Nothing)
    definitions
    Vector.!? 0

checkCommand :: Vector Definition -> Syntax.Command -> Typecheck Command
checkCommand definitions command =
  let tablesType =
        Type.record $
          Vector.mapMaybe
            ( \case
                Definition.Table Table{name, outFields} -> Just (name, Type.App (Type.Name "Relation") $ Type.record outFields)
            )
            definitions
      nameTypes = HashMap.singleton "tables" tablesType
   in case command of
        Syntax.Eval{value} -> do
          ty <- unknown
          value' <- zonkExpr =<< checkExpr nameTypes absurd value ty
          type_ <- zonk ty
          pure Eval{value = value', type_}
        Syntax.Insert{table = tableName, value} ->
          case lookupTable definitions tableName of
            Nothing ->
              throwError NotATable{table = tableName}
            Just table -> do
              value' <- zonkExpr =<< checkExpr mempty absurd value (Table.inputType table)
              pure Insert{table, value = value'}