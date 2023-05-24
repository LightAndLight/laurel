module Laurel.Eval (eval) where

import Bound.Scope (fromScope, mapScope)
import Bound.Var (unvar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (foldl', foldrM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import Laurel.Expr (Expr (..))
import Laurel.Value (Value)
import qualified Laurel.Value as Value

valueEq :: Applicative m => Value -> Value -> m Value
valueEq v1 v2 =
  case (v1, v2) of
    (Value.Relation a, Value.Relation b) ->
      pure . Value.Bool $ a == b
    (Value.Record a, Value.Record b) ->
      pure . Value.Bool $ a == b
    (Value.Int a, Value.Int b) ->
      pure . Value.Bool $ a == b
    (Value.Bool a, Value.Bool b) ->
      pure . Value.Bool $ a == b
    (Value.String a, Value.String b) ->
      pure . Value.Bool $ a == b
    (Value.Unit, Value.Unit) ->
      pure $ Value.Bool True
    _ ->
      pure $ Value.Bool False

eval :: MonadIO m => HashMap Text Value -> (a -> Value) -> Expr a -> m Value
eval context varValue expr =
  case expr of
    Name name ->
      case HashMap.lookup name context of
        Just value -> pure value
        Nothing -> error $ "name " <> show name <> " not in scope"
    Ctor name args -> do
      Value.Ctor name <$> traverse (eval context varValue) args
    Var var ->
      pure $ varValue var
    Lam _ body ->
      pure $ Value.Lam 1 (mapScope (\() -> 0) varValue body)
    App _ function args -> do
      function' <- eval context varValue function
      case function' of
        Value.Lam argCount body -> do
          when (argCount > length args)
            . error
            $ "lam applied to too many args, expected " <> show argCount <> ", got " <> show (length args)
          args' <- traverse (eval context varValue) args
          eval context (unvar (args' Vector.!) id) (fromScope body)
        _ -> error $ "expected lam, got " <> show function'
    Yield value -> do
      value' <- eval context varValue value
      pure . Value.Relation $ Value.fromVector [value']
    For _ _ collection body -> do
      collection' <- eval context varValue collection
      case collection' of
        Value.Relation values -> do
          values' <- traverse (\value -> eval context (unvar (\() -> value) varValue) $ fromScope body) values
          pure . Value.Relation $
            foldl'
              ( \acc value' ->
                  case value' of
                    Value.Relation values'' -> acc <> values''
                    _ -> error $ "expected relation, got " <> show value'
              )
              mempty
              values'
        _ -> error $ "expected relation, got " <> show collection'
    Where condition rest -> do
      condition' <- eval context varValue condition
      case condition' of
        Value.Bool b ->
          if b
            then eval context varValue rest
            else pure $ Value.Relation mempty
        _ -> error $ "expected bool, got " <> show condition'
    GroupBy _ _ collection projection -> do
      collection' <- eval context varValue collection
      case collection' of
        Value.Relation values -> do
          projection' <- eval context varValue projection
          case projection' of
            Value.Lam _ projection'' -> do
              Value.Map . fmap Value.Relation
                <$> foldrM
                  ( \value rest -> do
                      key <- eval context (unvar ([value] Vector.!) id) $ fromScope projection''
                      pure $ HashMap.insertWith (\new old -> old <> new) key (Value.fromVector [value]) rest
                  )
                  mempty
                  values
            _ ->
              error $ "expected function, got " <> show projection'
        _ ->
          error $ "expected relation, got " <> show collection'
    Dot _ record field -> do
      record' <- eval context varValue record
      case record' of
        Value.Record fields ->
          case HashMap.lookup field fields of
            Just value -> pure value
            _ -> error $ "record " <> show record' <> " missing field " <> show field
        _ -> error $ "expected record, got " <> show record'
    Splat record fields -> do
      record' <- eval context varValue record
      case record' of
        Value.Record actualFields -> do
          fields' <- for fields $ \field ->
            case HashMap.lookup field actualFields of
              Just value -> pure (field, value)
              _ -> error $ "record " <> show record' <> " missing field " <> show field
          pure . Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty fields'
        _ -> error $ "expected record, got " <> show record'
    Record fields -> do
      fields' <- (traverse . traverse) (eval context varValue) fields
      pure . Value.Record $ foldl' (\acc (key, value) -> HashMap.insert key value acc) mempty fields'
    Int i ->
      pure $ Value.Int i
    Bool b ->
      pure $ Value.Bool b
    String s ->
      pure $ Value.String s
    Equals a b -> do
      a' <- eval context varValue a
      b' <- eval context varValue b
      valueEq a' b'
    List items ->
      Value.List <$> traverse (eval context varValue) items