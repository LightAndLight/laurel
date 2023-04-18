{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Repl (run) where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import qualified Dblang.CSV.Run as Dblang.Csv.Run
import Dblang.Definition.Pretty (prettyDefinition)
import qualified Dblang.Eval
import qualified Dblang.Parse
import qualified Dblang.Postgres.Run
import Dblang.Run (Run (..), RunError)
import qualified Dblang.Run as RunError
import qualified Dblang.Syntax as Syntax
import qualified Dblang.Type as Type
import qualified Dblang.Typecheck as Typecheck
import qualified Dblang.Value as Value
import Dblang.Value.Pretty (pretty, prettyType)
import qualified Hasql.Connection as Postgres
import qualified Pretty
import Streaming (Of (..), Stream, hoist, lift, liftIO)
import Streaming.Chars.Text (StreamText (..))
import qualified Streaming.Prelude as Streaming
import qualified System.Console.ANSI as Terminal
import System.IO (
  BufferMode (..),
  Handle,
  hFlush,
  hGetChar,
  hIsEOF,
  hPutStr,
  hSetBuffering,
  hSetEcho,
 )
import Text.Parser.Combinators (eof)
import Text.Sage (parse)
import Prelude hiding (break)

mkRun :: Run IO
mkRun =
  Run
    { eval =
        \input ->
          runExceptT @(RunError Void) $ do
            syntax <-
              either (throwError . RunError.ParseError) pure $
                parse (Dblang.Parse.expr Syntax.Name <* eof) (StreamText input)

            (core, ty) <-
              either (throwError . RunError.TypeError) pure . Typecheck.runTypecheck $ do
                ty <- Typecheck.unknown
                core <- Typecheck.checkExpr mempty absurd syntax ty
                (,)
                  <$> Typecheck.zonkExpr core
                  <*> Typecheck.zonk ty

            value <- Dblang.Eval.eval mempty absurd core
            pure (value, ty)
    , typeOf =
        \input ->
          runExceptT @(RunError Void) $ do
            syntax <-
              either (throwError . RunError.ParseError) pure $
                parse (Dblang.Parse.expr Syntax.Name <* eof) (StreamText input)

            either (throwError . RunError.TypeError) pure . Typecheck.runTypecheck $ do
              ty <- Typecheck.unknown
              _core <- Typecheck.checkExpr mempty absurd syntax ty
              Typecheck.zonk ty
    , definitions =
        pure mempty
    }

run :: Handle -> Handle -> IO ()
run inputHandle outputHandle = do
  hSetEcho inputHandle False
  hSetBuffering inputHandle NoBuffering

  adapterRef <- newIORef mkRun
  outputs <- repl adapterRef <$> terminalInputs inputHandle

  Streaming.mapM_ (\content -> hPutStr outputHandle content *> hFlush outputHandle) outputs

repl :: IORef (Run IO) -> Stream (Of TerminalInput) IO () -> Stream (Of String) IO ()
repl adapterRef inputs = do
  Streaming.yield "Welcome to the dblang REPL. Type :quit to exit.\n"
  loop adapterRef inputs

loop :: IORef (Run IO) -> Stream (Of TerminalInput) IO () -> Stream (Of String) IO ()
loop adapterRef =
  go $ \continue break inputs -> do
    Streaming.yield "> "
    (inputs', line) <- readLine inputs
    case words line of
      [":quit"] ->
        break ()
      ":connect" : adapter : args -> do
        case adapter of
          "postgres" ->
            case parse (Dblang.Parse.expr Syntax.Name <* eof) (StreamText . Text.pack $ unwords args) of
              Left err ->
                Streaming.yield $ show err <> "\n"
              Right expr ->
                case Typecheck.runTypecheck $ Typecheck.checkExpr mempty absurd expr (Type.record [("host", Type.Name "String"), ("database", Type.Name "String")]) of
                  Left err ->
                    Streaming.yield $ show err <> "\n"
                  Right core -> do
                    value <- Dblang.Eval.eval mempty absurd core
                    case value of
                      Value.Record fields
                        | Just (Value.String host) <- fields HashMap.!? "host"
                        , Just (Value.String database) <- fields HashMap.!? "database" ->
                            do
                              result <-
                                liftIO . Postgres.acquire $
                                  Postgres.settings
                                    (Text.Encoding.encodeUtf8 host)
                                    5432
                                    ""
                                    ""
                                    (Text.Encoding.encodeUtf8 database)
                              case result of
                                Left err ->
                                  Streaming.yield $ show err <> "\n"
                                Right connection -> do
                                  liftIO $
                                    writeIORef adapterRef
                                      =<< Dblang.Postgres.Run.mkRun connection
                                  Streaming.yield "connected\n"
                      _ -> undefined
          "csv" -> do
            result <- liftIO $ Dblang.Csv.Run.mkRun (Vector.fromList args)
            case result of
              Left err ->
                Streaming.yield $ show err <> "\n"
              Right newAdapter -> do
                liftIO $ writeIORef adapterRef newAdapter
                Streaming.yield "connected\n"
          _ -> do
            Streaming.yield $ show ("error: unknown adapter " <> show adapter) <> "\n"
        continue inputs'
      ":type" : args -> do
        Run{typeOf} <- lift $ readIORef adapterRef
        result <- lift . typeOf $ Text.pack (unwords args)
        case result of
          Left err ->
            Streaming.yield $ show err <> "\n"
          Right ty ->
            Streaming.yield $ Text.unpack (prettyType ty) <> "\n"
        continue inputs'
      [":tables"] -> do
        Run{definitions = getDefinitions} <- liftIO $ readIORef adapterRef
        definitions <- liftIO getDefinitions
        Streaming.yield $
          Text.unpack
            ( Pretty.unlines . Pretty.vertically $
                Pretty.sepBy
                  (fmap prettyDefinition definitions)
                  (Pretty.line "")
            )
            <> "\n"
        continue inputs'
      _ -> do
        Run{eval} <- lift $ readIORef adapterRef
        result <- lift . eval $ Text.pack line
        case result of
          Left err ->
            Streaming.yield $ show err <> "\n"
          Right (value, ty) ->
            Streaming.yield $ Text.unpack (Pretty.unlines $ pretty value ty) <> "\n"
        continue inputs'
 where
  go :: Applicative m => (forall x. (s -> m x) -> (a -> m x) -> s -> m x) -> s -> m a
  go f = f (go f) pure

readLine ::
  Stream (Of TerminalInput) IO () ->
  Stream (Of String) IO (Stream (Of TerminalInput) IO (), String)
readLine inputs = do
  string :> inputs' <-
    Streaming.foldM
      ( \string terminalInput ->
          case terminalInput of
            CursorUp ->
              insertion "<<HISTORY PREVIOUS>>" string
            CursorDown ->
              insertion "<<HISTORY NEXT>>" string
            CursorBackward -> do
              Streaming.yield $ Terminal.cursorBackwardCode 1
              pure $ moveLeft string
            CursorForward -> do
              Streaming.yield $ Terminal.cursorForwardCode 1
              pure $ moveRight string
            KeyDelete ->
              delete string
            Char '\DEL' ->
              backspace string
            Char c ->
              insertion [c] string
      )
      (pure $ Input "" "")
      pure
      (hoist liftIO $ Streaming.break (== Char '\n') inputs)

  result <- lift $ Streaming.next inputs'
  inputs'' <-
    case result of
      Left () ->
        pure $ pure ()
      Right (_newline, inputs'') -> do
        Streaming.yield $ Terminal.clearFromCursorToLineEndCode <> string.right <> "\n"
        pure inputs''

  pure (inputs'', inputString string)
 where
  insertion :: String -> Input -> Stream (Of String) IO Input
  insertion new string = do
    let string' = insertLefts new string

    Streaming.yield $
      new
        <> Terminal.saveCursorCode
        <> Terminal.clearFromCursorToLineEndCode
        <> string'.right
        <> Terminal.restoreCursorCode

    pure string'

  backspace :: Input -> Stream (Of String) IO Input
  backspace string = do
    case deleteLeft string of
      Nothing ->
        pure string
      Just string' -> do
        Streaming.yield $
          Terminal.cursorBackwardCode 1
            <> Terminal.saveCursorCode
            <> Terminal.clearFromCursorToLineEndCode
            <> string'.right
            <> Terminal.restoreCursorCode

        pure string'

  delete :: Input -> Stream (Of String) IO Input
  delete string = do
    let string' = deleteRight string

    Streaming.yield $
      Terminal.saveCursorCode
        <> Terminal.clearFromCursorToLineEndCode
        <> string'.right
        <> Terminal.restoreCursorCode

    pure string'

data Input = Input {left :: [Char], right :: [Char]}
  deriving (Eq, Show)

insertLeft :: Char -> Input -> Input
insertLeft c (Input left right) = Input (c : left) right

insertLefts :: String -> Input -> Input
insertLefts string input = foldl (flip insertLeft) input string

deleteLeft :: Input -> Maybe Input
deleteLeft (Input left right) =
  case left of
    [] ->
      Nothing
    _ : left' ->
      Just $ Input left' right

deleteRight :: Input -> Input
deleteRight input@(Input left right) =
  case right of
    [] ->
      input
    _ : right' ->
      Input left right'

moveLeft :: Input -> Input
moveLeft input@(Input left right) =
  case left of
    [] -> input
    l : left' ->
      Input left' (l : right)

moveRight :: Input -> Input
moveRight input@(Input left right) =
  case right of
    [] -> input
    r : right' ->
      Input (r : left) right'

inputString :: Input -> String
inputString (Input left right) = reverse left <> right

data TerminalInput
  = CursorBackward
  | CursorForward
  | CursorUp
  | CursorDown
  | KeyDelete
  | Char Char
  deriving (Eq, Show)

nextTerminalInput :: IORef [Char] -> Handle -> IO TerminalInput
nextTerminalInput buffer input = do
  c1 <- nextChar
  case c1 of
    '\ESC' -> do
      c2 <- hGetChar input
      case c2 of
        '[' -> do
          c3 <- hGetChar input
          case c3 of
            'A' -> do
              pure CursorUp
            'B' -> do
              pure CursorDown
            'C' -> do
              pure CursorForward
            'D' -> do
              pure CursorBackward
            '3' -> do
              c4 <- hGetChar input
              case c4 of
                '~' -> do
                  pure KeyDelete
                _ -> do
                  unNextChar c4
                  unNextChar c3
                  unNextChar c2
                  pure $ Char c1
            _ -> do
              unNextChar c3
              unNextChar c2
              pure $ Char c1
        _ -> do
          unNextChar c2
          pure $ Char c1
    _ ->
      pure $ Char c1
 where
  nextChar = do
    contents <- readIORef buffer
    case contents of
      [] -> hGetChar input
      c : rest -> do
        writeIORef buffer rest
        pure c

  unNextChar c = modifyIORef buffer (c :)

terminalInputs :: Handle -> IO (Stream (Of TerminalInput) IO ())
terminalInputs handle = do
  buffer <- newIORef []
  pure $
    Streaming.unfoldr
      ( \input -> do
          isEof <- hIsEOF input
          if isEof
            then pure $ Left ()
            else do
              next <- nextTerminalInput buffer input
              pure $ Right (next, input)
      )
      handle