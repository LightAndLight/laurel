{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dblang.Typecheck (Typecheck, runTypecheck, Error (..), checkDefinition, checkExpr) where

import Bound (fromScope, toScope)
import Bound.Var (unvar)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (lift)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Vector as Vector
import Dblang.Definition (Definition)
import Dblang.Expr (Expr (..))
import qualified Dblang.Syntax as Syntax
import Dblang.Type (Type)
import qualified Dblang.Type as Type

checkDefinition :: Definition -> m ()
checkDefinition = error "TODO: checkDefinition"

data Error
  = NotInScope {name :: Text}
  | TypeMismatch {expected :: Type, actual :: Type}
  | Occurs {meta :: Int, type_ :: Type}
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