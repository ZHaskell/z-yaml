{-|
Module      : Z.Data.YAML.FFI
Description : LibYAML bindings
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Simple YAML codec using <https://libyaml.docsforge.com/ libYAML> and JSON's 'JSON' utilities.
The design choice to make things as simple as possible since YAML is a complex format, there're some limitations using this approach:

* Does not support complex keys.
* Does not support multiple doucments in one file.

@
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

import           GHC.Generics
import qualified Z.Data.Text  as T
import qualified Z.Data.YAML  as YAML

data Person = Person
    { name  :: T.Text
    , age   :: Int
    , magic :: Bool
    }
  deriving (Show, Generic)
  deriving anyclass (YAML.JSON)

> YAML.decode @[Person] "- name: Erik Weisz\\n  age: 52\\n  magic: True\\n"
> Right [Person {name = "Erik Weisz", age = 52, magic = True}]
@

-}


module Z.Data.YAML
  ( -- * Decode and encode using YAML
    decode
  , encode
  , readYAMLFile
  , writeYAMLFile
  -- * Streaming parser and builder
  , initParser, initFileParser
  , parseSingleDoucment
  , parseAllDocuments
  , initEmitter, initFileEmitter
  , buildSingleDocument
  , buildValue
  -- * Errors
  , YAMLError(..)
  , YAMLParseError(..)
  , ConvertError(..)
  , DecodeError
  -- * Re-Exports
  , JSON(..)
  , Value(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Bits              ((.|.), unsafeShiftL)
import           Data.IORef
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.Scientific        as Sci
import           GHC.Generics           (Generic)
import           System.IO.Unsafe
import           Z.Data.ASCII
import qualified Z.Data.Parser          as P
import qualified Z.Data.Vector          as V
import qualified Z.Data.Text            as T
import           Z.Data.JSON            (JSON(..), Value(..), ConvertError, convertValue)
import           Z.Data.YAML.FFI
import qualified Z.Data.Vector.FlatMap  as FM
import qualified Z.Data.Builder         as B
import           Z.Data.CBytes          (CBytes)
import           Z.Data.YAML.FFI
import           Z.IO

type DecodeError = Either YAMLError ConvertError

-- | Decode a 'JSON' instance from a YAML file.
readYAMLFile :: forall a. (HasCallStack, JSON a) => CBytes -> IO a
readYAMLFile p = unwrap "EPARSE" =<< withResource (initFileParser p) (\ src -> do
    r <- try (parseSingleDoucment src)
    case r of
        Left (e :: YAMLError) -> return (Left (Left e))
        Right r' -> case (convertValue r' :: Either ConvertError a) of
            Left e' -> return (Left (Right e'))
            Right v -> return (Right v))

-- | Decode a 'JSON' instance from YAML bytes.
decode :: forall a .(HasCallStack, JSON a) => V.Bytes -> Either DecodeError a
decode bs = unsafePerformIO . withResource (initParser bs) $ \ src -> do
    r <- try (parseSingleDoucment src)
    case r of
        Left e -> return (Left (Left e))
        Right r' -> case (convertValue r' :: Either ConvertError a) of
            Left e' -> return (Left (Right e'))
            Right v -> return (Right v)

-- | Encode a 'JSON' instance to YAML file.
writeYAMLFile :: (HasCallStack, JSON a) => YAMLFormatOpts -> CBytes -> a -> IO ()
writeYAMLFile opts p x = withResource (initFileEmitter opts p) $ \ sink ->
    buildSingleDocument sink (toValue x)

-- | Encode a 'JSON' instance as UTF8 YAML text.
encode :: (HasCallStack, JSON a) => YAMLFormatOpts -> a -> T.Text
encode opts x = unsafePerformIO . withResource (initEmitter opts) $ \ (p, sink) -> do
    buildSingleDocument sink (toValue x)
    getEmitterResult p

--------------------------------------------------------------------------------

-- | Parse a single YAML document, throw 'OtherYAMLError' if multiple documents are met.
parseSingleDoucment :: HasCallStack => IO (Maybe MarkedEvent) -> IO Value
parseSingleDoucment src = do
    docs <- parseAllDocuments src
    case docs of
        [] -> return Null
        [doc] -> return doc
        _ -> throwIO (OtherYAMLError "multiple YAML documents")

-- | Parse all YAML documents.
parseAllDocuments :: HasCallStack => IO (Maybe MarkedEvent) -> IO [Value]
parseAllDocuments src = do
    me <- src
    case me of
        Just (MarkedEvent EventStreamStart _ _) -> do
            as <- newIORef HM.empty
            catch (runReaderT parseDocs (src, as)) $ \ (e :: YAMLParseError) ->
                throwYAMLError e
        Just me' -> throwYAMLError (UnexpectedEvent me')
        -- empty file input, comment only string/file input
        _ -> return []
  where
    parseDocs = do
        me <- pullEvent
        case me of
            MarkedEvent EventStreamEnd _ _      -> return []
            MarkedEvent EventDocumentStart _ _  -> do
                res <- parseValue =<< pullEvent
                me' <- pullEvent
                case me' of
                    MarkedEvent EventDocumentEnd _ _ ->
                        (res :) <$> parseDocs
                    me'' -> throwParserIO (UnexpectedEvent me'')


type ParserIO = ReaderT (IO (Maybe MarkedEvent), IORef (HM.HashMap T.Text Value)) IO

pullEvent :: ParserIO MarkedEvent
pullEvent = do
    (src, _) <- ask
    liftIO $ do
        me <- src
        case me of Just e -> return e
                   _ -> throwIO UnexpectedEventEnd

throwParserIO :: YAMLParseError -> ParserIO a
throwParserIO = liftIO . throwIO

defineAnchor :: T.Text -> Value -> ParserIO ()
defineAnchor key value = unless (T.null key) $ do
    (_, mref) <- ask
    liftIO $ modifyIORef' mref (HM.insert key value)

lookupAlias :: MarkedEvent -> T.Text -> ParserIO Value
lookupAlias me key = do
    (_, mref) <- ask
    liftIO $ do
        m <- readIORef mref
        case HM.lookup key m of
            Just v -> return v
            _ -> throwIO (UnknownAlias me)

textToValue :: ScalarStyle -> Tag -> T.Text -> Value
textToValue SingleQuoted _ t = String t
textToValue DoubleQuoted _ t = String t
textToValue _ StrTag t       = String t
textToValue Folded _ t       = String t
textToValue _ _ t
    | t `elem` ["null", "Null", "NULL", "~", ""] = Null
    | t `elem` ["y", "Y", "yes", "on", "true", "YES", "ON", "TRUE", "Yes", "On", "True"]    = Bool True
    | t `elem` ["n", "N", "no", "off", "false", "NO", "OFF", "FALSE", "No", "Off", "False"] = Bool False
    | Right x <- textToScientific t = Number x
    | otherwise = String t

textToScientific :: T.Text -> Either P.ParseError Sci.Scientific
textToScientific = P.parse' (num <* P.endOfInput) . T.getUTF8Bytes
  where
    num = (fromInteger <$> (P.bytes "0x" *> P.hex_ @Integer))
      <|> (fromInteger <$> (P.bytes "0o" *> octal))
      <|> P.scientific

    octal = V.foldl' step 0 <$> P.takeWhile1 (\ w -> w >= DIGIT_0 && w < DIGIT_0+8)
    step a c = (a `unsafeShiftL` 3) .|. fromIntegral (c - DIGIT_0)

parseValue :: MarkedEvent -> ParserIO Value
parseValue me@(MarkedEvent e startMark endMark) =
    case e of
        EventScalar anchor v tag style -> do
            let !v' = textToValue style tag v
            defineAnchor anchor v'
            return v'
        EventSequenceStart anchor _ _  -> do
            !v <- parseSequence
            defineAnchor anchor v
            return v
        EventMappingStart anchor _ _   -> do
            !v <- parseMapping
            defineAnchor anchor v
            return v
        EventAlias anchor              -> lookupAlias me anchor
        _ -> throwParserIO (UnexpectedEvent me)

parseSequence :: ParserIO Value
parseSequence = Array . V.packR <$> go []
  where
    go acc = do
        e <- pullEvent
        case e of
            MarkedEvent EventSequenceEnd _ _ -> return acc
            _ -> do
                o <- parseValue e
                go (o:acc)

parseMapping :: ParserIO Value
parseMapping = Object . V.packR <$> go []
  where
    go acc = do
        me <- pullEvent
        case me of
            MarkedEvent EventMappingEnd _ _ -> return acc
            MarkedEvent e startMark endMark -> do
                key <- case e of
                    EventScalar anchor v tag style ->
                        case textToValue style tag v of
                            k@(String k') -> do
                                defineAnchor anchor k
                                return k'
                            _ -> throwParserIO (NonStringKey me)

                    EventAlias anchor -> do
                        m <- lookupAlias me anchor
                        case m of
                            String k -> return k
                            _ -> throwParserIO (NonStringKeyAlias me)
                    e -> throwParserIO (UnexpectedEvent me)

                value <- parseValue =<< pullEvent

                -- overidding
                if key == "<<"
                then case value of
                    -- overide a mapping literal
                    Object kvs  -> go (V.unpackR kvs ++ acc)
                    -- overide a mapping list
                    Array vs -> go (V.foldr' mergeMapping acc vs)
                    v          ->  throwParserIO (UnexpectedEvent me)

                else go ((key, value):acc)

    -- ignore non-object
    mergeMapping  (Object o) acc = acc ++ V.unpackR o
    mergeMapping  v          acc = acc

--------------------------------------------------------------------------------

-- | Write a value as a YAML document stream.
--
buildSingleDocument :: HasCallStack => (Event -> IO ()) -> Value -> IO ()
buildSingleDocument sink v = do
    sink EventStreamStart
    sink EventDocumentStart
    buildValue sink v
    sink EventDocumentEnd
    void $ sink EventStreamEnd

-- | Write a value as a stream of 'Event's(without document start\/end, stream start\/end).
--
buildValue :: HasCallStack => (Event -> IO ()) -> Value -> IO ()
buildValue sink (Array vs) = do
    sink (EventSequenceStart "" NoTag AnySequence)
    mapM_ (buildValue sink) (V.unpack vs)
    void $ sink EventSequenceEnd

buildValue sink (Object o) = do
    sink (EventMappingStart "" NoTag AnyMapping)
    mapM_ encodeKV (V.unpack o)
    void $ sink EventMappingEnd
  where
    encodeKV (k, v) = buildValue sink (String k) >> buildValue sink v

buildValue sink (String s) = void $ sink (EventScalar "" s NoTag (stringStyle s))
  where
    stringStyle s
        | (_, Just _) <- (== '\n') `T.find` s   = Literal
        | isSpecialString s                     = SingleQuoted
        | otherwise                             = PlainNoTag

    isSpecialString s = s `HS.member` specialStrings || isNumeric s
    specialStrings = HS.fromList $ T.words
        "y Y yes Yes YES n N no No NO true True TRUE false False FALSE on On ON off Off OFF null Null NULL ~ *"
    isNumeric = either (const False) (const True) . textToScientific

buildValue sink Null         = void $ sink (EventScalar "" "null" NullTag PlainNoTag)
buildValue sink (Bool True)  = void $ sink (EventScalar "" "true" BoolTag PlainNoTag)
buildValue sink (Bool False) = void $ sink (EventScalar "" "false" BoolTag PlainNoTag)
buildValue sink (Number s)   = do
    let builder
            -- Special case the 0 exponent to remove the trailing .0
            | Sci.base10Exponent s == 0 = B.integer $ Sci.coefficient s
            | otherwise = B.scientific s
        t = B.unsafeBuildText builder
    void $ sink (EventScalar "" t IntTag PlainNoTag)
