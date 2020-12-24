{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Z.Data.YAML.FFI
Description : LibYAML bindings
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

LibYAML bindings, which provide streaming YAML read & write.

-}

module Z.Data.YAML.FFI
    ( -- * The event stream
      MarkedEvent(..)
    , Mark (..)
    , Event (..)
    , Tag(..)
    , Anchor
      -- * Decoding
    , initParser
    , initFileParser
      -- * Encoding
    , YAMLFormatOpts(..)
    , initEmitter
    , initFileEmitter
    , getEmitterResult
    , defaultYAMLFormatOpts
    , renderScalarTags
    , renderAllTags
    , renderNoTags
    , renderUriTags
    -- * Constants
    , ScalarStyle
    , pattern Any          
    , pattern Plain        
    , pattern SingleQuoted 
    , pattern DoubleQuoted 
    , pattern Literal      
    , pattern Folded       
    , pattern PlainNoTag   
    , SequenceStyle
    , pattern AnySequence  
    , pattern BlockSequence
    , pattern FlowSequence 
    , MappingStyle
    , pattern AnyMapping
    , pattern BlockMapping
    , pattern FlowMapping 
    , TagRender
    , pattern Explicit 
    , pattern Implicit
    -- * Exception type
    , LibYAMLException (..)
    ) where

import Control.Applicative
import Control.Exception (mask_, throwIO, Exception, finally)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Bits ((.|.))
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Prelude hiding (pi)
import qualified Z.Data.CBytes      as CB
import Z.Foreign
import Z.IO
import qualified Z.IO.FileSystem    as FS
import qualified Z.Data.Vector      as V
import qualified Z.Data.Text.Base   as T
import           Z.Data.Text.ShowT  (ShowT)
import           Z.Data.JSON        (EncodeJSON, FromValue, ToValue)

#include "yaml.h"

type Anchor = T.Text

data Event =
      EventStreamStart   
    | EventStreamEnd     
    | EventDocumentStart 
    | EventDocumentEnd   
    | EventAlias          !Anchor
    | EventScalar         !Anchor !T.Text !Tag !ScalarStyle 
    | EventSequenceStart  !Anchor !Tag !SequenceStyle 
    | EventSequenceEnd   
    | EventMappingStart   !Anchor !Tag !MappingStyle 
    | EventMappingEnd    
    deriving (Show, Ord, Eq, Generic)
    deriving anyclass (ShowT, EncodeJSON, FromValue, ToValue)

data MarkedEvent = MarkedEvent 
    { markedEvent :: !Event
    , startMark :: !Mark
    , endMark :: !Mark
    }
    deriving (Show, Ord, Eq, Generic)
    deriving anyclass (ShowT, EncodeJSON, FromValue, ToValue)

-- | The pointer position
data Mark = Mark 
    { yamlIndex  :: {-# UNPACK #-} !Int
    , yamlLine   :: {-# UNPACK #-} !Int
    , yamlColumn :: {-# UNPACK #-} !Int 
    }
    deriving (Show, Ord, Eq, Generic)
    deriving anyclass (ShowT, EncodeJSON, FromValue, ToValue)

-- | Style for scalars - e.g. quoted / folded
-- 
type ScalarStyle = CInt
pattern Any, Plain, SingleQuoted, DoubleQuoted, Literal, Folded, PlainNoTag :: ScalarStyle
pattern Any           = 0
pattern Plain         = 1 
pattern SingleQuoted  = 2 
pattern DoubleQuoted  = 3 
pattern Literal       = 4 
pattern Folded        = 5 
pattern PlainNoTag    = 6 

-- | Style for sequences - e.g. block or flow
-- 
type SequenceStyle = CInt
pattern AnySequence, BlockSequence, FlowSequence :: SequenceStyle
pattern AnySequence   = 0
pattern BlockSequence = 1 
pattern FlowSequence  = 2

-- | Style for mappings - e.g. block or flow
-- 
type MappingStyle = CInt 
pattern AnyMapping, BlockMapping, FlowMapping :: MappingStyle
pattern AnyMapping   = 0 
pattern BlockMapping = 1
pattern FlowMapping  = 2

data Tag = StrTag
         | FloatTag
         | NullTag
         | BoolTag
         | SetTag
         | IntTag
         | SeqTag
         | MapTag
         | UriTag T.Text
         | NoTag
    deriving (Show, Ord, Eq, Generic)
    deriving anyclass (ShowT, EncodeJSON, FromValue, ToValue)

tagToCBytes :: Tag -> CB.CBytes
tagToCBytes StrTag = "tag:yaml.org,2002:str"
tagToCBytes FloatTag = "tag:yaml.org,2002:float"
tagToCBytes NullTag = "tag:yaml.org,2002:null"
tagToCBytes BoolTag = "tag:yaml.org,2002:bool"
tagToCBytes SetTag = "tag:yaml.org,2002:set"
tagToCBytes IntTag = "tag:yaml.org,2002:int"
tagToCBytes SeqTag = "tag:yaml.org,2002:seq"
tagToCBytes MapTag = "tag:yaml.org,2002:map"
tagToCBytes (UriTag s) = CB.fromText s
tagToCBytes NoTag = ""

bytesToTag :: V.Bytes -> Tag
bytesToTag "tag:yaml.org,2002:str" = StrTag
bytesToTag "tag:yaml.org,2002:float" = FloatTag
bytesToTag "tag:yaml.org,2002:null" = NullTag
bytesToTag "tag:yaml.org,2002:bool" = BoolTag
bytesToTag "tag:yaml.org,2002:set" = SetTag
bytesToTag "tag:yaml.org,2002:int" = IntTag
bytesToTag "tag:yaml.org,2002:seq" = SeqTag
bytesToTag "tag:yaml.org,2002:map" = MapTag
bytesToTag "" = NoTag
bytesToTag s = UriTag (T.validate s)

data LibYAMLException
    = ParseEventException CB.CBytes CB.CBytes Mark CallStack  -- ^ problem, context, mark
    | ParseAliasEventWithEmptyAnchor Mark Mark CallStack
    | EmitEventException Event CInt CallStack
    | EmitAliasEventWithEmptyAnchor CallStack
    deriving Show

instance Exception LibYAMLException

data ParserStruct
foreign import ccall unsafe "hs_yaml.c hs_init_yaml_parser" hs_init_yaml_parser :: IO (Ptr ParserStruct)
foreign import ccall unsafe "hs_yaml.c hs_free_yaml_parser" hs_free_yaml_parser :: Ptr ParserStruct -> IO ()

data EventStruct
foreign import ccall unsafe yaml_parser_set_input_string :: Ptr ParserStruct -> Ptr Word8 -> CSize -> IO ()
foreign import ccall unsafe yaml_parser_set_input_file :: Ptr ParserStruct -> Ptr File -> IO ()
foreign import ccall unsafe yaml_parser_parse :: Ptr ParserStruct -> MBA## EventStruct -> IO CInt
foreign import ccall unsafe yaml_event_delete :: MBA## EventStruct -> IO ()

-- | Create a source that yields marked events from a piece of YAML bytes.
--
initParser :: HasCallStack => V.Bytes -> Resource (Source MarkedEvent)
initParser bs 
    | V.null bs = return BIO{ pull = return Nothing }
    | otherwise = do
        (pparser, bs', bio) <- initResource 
            (do pparser <- throwOOMIfNull hs_init_yaml_parser
                bs' <- pinPrimVector bs
                withPrimVectorSafe bs' $ \ bptr blen -> do
                    yaml_parser_set_input_string pparser bptr (fromIntegral blen)
                return (pparser, bs', BIO{ pull = peekParserEvent pparser }))
            (\ (pparser, bs', _) -> do
                hs_free_yaml_parser pparser
                touch bs')
        return bio

-- | Create a source that yields marked events from a piece of YAML bytes.
--
initFileParser :: HasCallStack => CB.CBytes -> Resource (Source MarkedEvent)
initFileParser p = do
    (pparser, file, bio) <- initResource 
        (do pparser <- throwOOMIfNull hs_init_yaml_parser
            (f, _) <- acquire $ FS.initFile p FS.O_RDONLY FS.DEFAULT_MODE
            fd <- FS.getFileFD f
            file <-   CB.withCBytesUnsafe "r" (fdopen fd)
            yaml_parser_set_input_file pparser file
            return (pparser, file, BIO{ pull = peekParserEvent pparser }))
        (\ (pparser, file, _) -> do
            hs_free_yaml_parser pparser
            fclose file)
    return bio

-- | Parse a single event from YAML parser.
peekParserEvent :: HasCallStack => Ptr ParserStruct -> IO (Maybe MarkedEvent)
peekParserEvent parser = do
    (_, me) <- allocBytesUnsafe (#size yaml_event_t) $ \ pe -> do
        res <- yaml_parser_parse parser pe
        flip finally (yaml_event_delete pe) $
            if res == 0
            then do
                problem <- CB.fromCString =<< (#peek yaml_parser_t, problem) parser
                context <- CB.fromCString =<< (#peek yaml_parser_t, context) parser
                i :: CUInt <- (#peek yaml_parser_t, problem_mark.index) parser
                l :: CUInt <- (#peek yaml_parser_t, problem_mark.line) parser
                c :: CUInt <- (#peek yaml_parser_t, problem_mark.column) parser
                let problemMark = Mark (fromIntegral i) (fromIntegral l) (fromIntegral c)
                throwIO (ParseEventException problem context problemMark callStack)
            else peekEvent pe
    return me
  where
    readAnchor :: Int -> MBA## EventStruct -> IO Anchor
    readAnchor off pe = do
        p <- peekMBA pe off 
        if p == nullPtr 
        then return T.empty
        else T.Text <$> fromNullTerminated p

    readStyle :: Int -> MBA## EventStruct -> IO CInt
    readStyle off pe = peekMBA pe off

    readTag :: Int -> MBA## EventStruct -> IO Tag
    readTag off pe = do
        p <- peekMBA pe off 
        if p == nullPtr 
        then return NoTag
        else bytesToTag <$!> fromNullTerminated p

    peekEvent :: HasCallStack => MBA## EventStruct -> IO (Maybe MarkedEvent)
    peekEvent pe = do
        et <- peekMBA pe (#offset yaml_event_t, type)

        si :: CUInt <- peekMBA pe (#offset yaml_event_t, start_mark.index) 
        sl :: CUInt <- peekMBA pe (#offset yaml_event_t, start_mark.line) 
        sc :: CUInt <- peekMBA pe (#offset yaml_event_t, start_mark.column) 
        ei :: CUInt <- peekMBA pe (#offset yaml_event_t, end_mark.index) 
        el :: CUInt <- peekMBA pe (#offset yaml_event_t, end_mark.line) 
        ec :: CUInt <- peekMBA pe (#offset yaml_event_t, end_mark.column) 
        let startMark = Mark (fromIntegral si) (fromIntegral sl) (fromIntegral sc)
            endMark = Mark (fromIntegral ei) (fromIntegral el) (fromIntegral ec)
            returnMarked e = return (Just (MarkedEvent e startMark endMark))
        case (et :: CInt) of
            (#const YAML_NO_EVENT)              -> return Nothing
            (#const YAML_STREAM_START_EVENT)    -> returnMarked EventStreamStart
            (#const YAML_STREAM_END_EVENT)      -> returnMarked EventStreamEnd
            (#const YAML_DOCUMENT_START_EVENT)  -> returnMarked EventDocumentStart
            (#const YAML_DOCUMENT_END_EVENT)    -> returnMarked EventDocumentEnd
            (#const YAML_ALIAS_EVENT) -> do
                yanchor <- peekMBA pe (#offset yaml_event_t, data.alias.anchor)
                anchor <- if yanchor == nullPtr
                          then throwIO (ParseAliasEventWithEmptyAnchor startMark endMark callStack)
                          else fromNullTerminated yanchor
                returnMarked (EventAlias (T.Text anchor))
            (#const YAML_SCALAR_EVENT) -> do
                anchor <- readAnchor (#offset yaml_event_t, data.scalar.anchor) pe
                yvalue <- peekMBA pe (#offset yaml_event_t, data.scalar.value)
                ylen   <- peekMBA pe (#offset yaml_event_t, data.scalar.length)
                bs <- fromPtr yvalue (fromIntegral (ylen :: CULong))
                tag <- readTag (#offset yaml_event_t, data.scalar.tag) pe
                style <- readStyle (#offset yaml_event_t, data.scalar.style) pe
                returnMarked (EventScalar anchor (T.Text bs) tag style)
            (#const YAML_SEQUENCE_START_EVENT) -> do
                anchor <- readAnchor (#offset yaml_event_t, data.sequence_start.anchor) pe
                tag <- readTag (#offset yaml_event_t, data.sequence_start.tag) pe
                style <- readStyle (#offset yaml_event_t, data.sequence_start.style) pe
                returnMarked (EventSequenceStart anchor tag style)
            (#const YAML_SEQUENCE_END_EVENT)    -> returnMarked EventSequenceEnd
            (#const YAML_MAPPING_START_EVENT) -> do
                anchor <- readAnchor (#offset yaml_event_t, data.mapping_start.anchor) pe
                tag <- readTag (#offset yaml_event_t, data.mapping_start.tag) pe
                style <- readStyle (#offset yaml_event_t, data.mapping_start.style) pe
                returnMarked (EventMappingStart anchor tag style)
            (#const YAML_MAPPING_END_EVENT) -> returnMarked EventMappingEnd


--------------------------------------------------------------------------------
-- Emitter

data EmitterStruct

foreign import ccall unsafe "hs_yaml.c hs_init_yaml_emitter"
    hs_init_yaml_emitter :: CInt -> CInt -> CInt -> IO (Ptr EmitterStruct)

foreign import ccall unsafe "hs_yaml.c hs_free_yaml_emitter"
    hs_free_yaml_emitter :: Ptr EmitterStruct -> IO ()

foreign import ccall unsafe "hs_yaml.c hs_init_yaml_emitter_file"
    hs_init_yaml_emitter_file :: Ptr File -> CInt -> CInt -> CInt -> IO (Ptr EmitterStruct)

foreign import ccall unsafe "hs_yaml.c hs_free_yaml_emitter_file"
    hs_free_yaml_emitter_file :: Ptr EmitterStruct -> IO ()

foreign import ccall unsafe "hs_yaml.c hs_get_yaml_emitter_length"
    hs_get_yaml_emitter_length :: Ptr EmitterStruct -> IO CSize

foreign import ccall unsafe "hs_yaml.c hs_copy_yaml_emitter_result"
    hs_copy_yaml_emitter_result :: Ptr EmitterStruct -> MBA## Word8 -> CSize -> IO ()

foreign import ccall unsafe yaml_emitter_emit :: Ptr EmitterStruct -> MBA## EventStruct -> IO CInt

foreign import ccall unsafe yaml_stream_start_event_initialize :: MBA## EventStruct -> CInt -> IO CInt

foreign import ccall unsafe yaml_stream_end_event_initialize :: MBA## EventStruct -> IO CInt

foreign import ccall unsafe "hs_yaml.c hs_yaml_scalar_event_initialize"
    hs_yaml_scalar_event_initialize
        :: MBA## EventStruct
        -> BA## Word8 -- anchor
        -> BA## Word8 -- tag
        -> BA## Word8 -- value
        -> CInt       -- offset
        -> CInt       -- length
        -> CInt       -- plain_implicit
        -> CInt       -- quoted_implicit
        -> CInt       -- style
        -> IO CInt

foreign import ccall unsafe "hs_yaml.c hs_yaml_document_start"
    hs_yaml_document_start :: MBA## EventStruct -> IO CInt

foreign import ccall unsafe yaml_document_end_event_initialize :: MBA## EventStruct -> CInt -> IO CInt

foreign import ccall unsafe "hs_yaml.c hs_yaml_sequence_start_event_initialize"
    hs_yaml_sequence_start_event_initialize
        :: MBA## EventStruct
        -> BA## Word8  -- anchor
        -> BA## Word8  -- tag
        -> CInt
        -> CInt
        -> IO CInt

foreign import ccall unsafe yaml_sequence_end_event_initialize :: MBA## EventStruct -> IO CInt

foreign import ccall unsafe "hs_yaml.c hs_yaml_mapping_start_event_initialize"
    hs_yaml_mapping_start_event_initialize
        :: MBA## EventStruct
        -> BA## Word8
        -> BA## Word8
        -> CInt
        -> CInt
        -> IO CInt

foreign import ccall unsafe yaml_mapping_end_event_initialize :: MBA## EventStruct -> IO CInt

foreign import ccall unsafe yaml_alias_event_initialize :: MBA## EventStruct -> BA## Word8 -> IO CInt

-- | Make a new YAML event sink, whose result can be fetched via 'getEmitterResult'.
--
initEmitter :: HasCallStack => YAMLFormatOpts -> Resource (Ptr EmitterStruct, Sink Event) 
initEmitter fopts@YAMLFormatOpts{..} = do
    p <- initResource 
        (do let canonical = if yamlFormatCanonical then 1 else 0
            throwOOMIfNull (hs_init_yaml_emitter canonical
                (fromIntegral yamlFormatIndent) (fromIntegral yamlFormatWidth)))
        hs_free_yaml_emitter
    return (p, BIO {
        push = \ e -> emitEvent p fopts e >> return Nothing
    ,   pull = return Nothing
    })

-- | Make a new YAML event sink, whose result are written to a file.
--
-- Note the file will be opened in @'FS.O_APPEND' .|. 'FS.O_CREAT' .|. 'FS.O_WRONLY'@ mode,
-- bytes will be written after the end of the original file if there'are old bytes.
initFileEmitter :: HasCallStack => YAMLFormatOpts -> CB.CBytes -> Resource (Sink Event) 
initFileEmitter fopts@YAMLFormatOpts{..} p = do
    (pemitter, file) <- initResource
        (do (f, _) <- acquire $ FS.initFile p (FS.O_APPEND .|. FS.O_CREAT .|. FS.O_WRONLY) FS.DEFAULT_MODE
            fd <- FS.getFileFD f
            file <- CB.withCBytesUnsafe "w" (fdopen fd)
            let canonical = if yamlFormatCanonical then 1 else 0
            pemitter <- throwOOMIfNull (hs_init_yaml_emitter_file file canonical
                (fromIntegral yamlFormatIndent) (fromIntegral yamlFormatWidth))
            return (pemitter, file))
            (\ (pemitter, file) -> do
            hs_free_yaml_emitter_file pemitter
            fclose file)
    return BIO {
        push = \ e -> emitEvent pemitter fopts e >> return Nothing
    ,   pull = return Nothing
    }

-- | Fetch YAML emitter's building buffer.
--
getEmitterResult :: Ptr EmitterStruct -> IO T.Text 
getEmitterResult pemitter = do
    l <- hs_get_yaml_emitter_length pemitter
    (bs,_) <- allocBytesUnsafe (fromIntegral l) $ \ p -> hs_copy_yaml_emitter_result pemitter p l
    return (T.Text bs)

-- | Push a single YAML event to emitter.
--
emitEvent :: HasCallStack => Ptr EmitterStruct -> YAMLFormatOpts -> Event -> IO ()
emitEvent pemitter fopts e = void . allocBytesUnsafe (#size yaml_event_t) $ \ pe -> do
    ret <- case e of
        EventStreamStart   -> yaml_stream_start_event_initialize pe (#const YAML_ANY_ENCODING)
        EventStreamEnd     -> yaml_stream_end_event_initialize pe
        EventDocumentStart -> hs_yaml_document_start pe
        EventDocumentEnd   -> yaml_document_end_event_initialize pe 1
        EventScalar anchor t tag style0 -> 
            withPrimVectorUnsafe (T.getUTF8Bytes t) $ \ pvalue off len -> 
                withAnchor anchor $ \ panchor -> 
                    withTag tag $ \ ptag -> do
                        let pi0 = tagsImplicit e
                            (pi, style) = case style0 of
                                PlainNoTag -> (1,   Plain)
                                x          -> (pi0, x)
                        hs_yaml_scalar_event_initialize
                            pe
                            panchor -- anchor
                            ptag    -- tag
                            pvalue  -- value
                            (fromIntegral off)   -- offset
                            (fromIntegral len)   -- length
                            (if T.null anchor then pi else 0)   -- plain_implicit
                            pi      -- quoted_implicit
                            style   -- style

        EventSequenceStart anchor tag style ->
            withAnchor anchor $ \ panchor -> 
                withTag tag $ \ ptag ->
                    hs_yaml_sequence_start_event_initialize
                        pe
                        panchor
                        ptag
                        (tagsImplicit e)
                        style

        EventSequenceEnd -> yaml_sequence_end_event_initialize pe

        EventMappingStart anchor tag style ->
            withAnchor anchor $ \ panchor ->
                withTag tag $ \ ptag -> 
                    hs_yaml_mapping_start_event_initialize pe panchor ptag (tagsImplicit e) style

        EventMappingEnd -> yaml_mapping_end_event_initialize pe

        EventAlias anchor ->
            if T.null anchor
            then throwIO (EmitAliasEventWithEmptyAnchor callStack)
            else withAnchor anchor (yaml_alias_event_initialize pe)

    if (ret /= 1) 
    then throwIO (EmitEventException e ret callStack)
    else do
        ret' <- yaml_emitter_emit pemitter pe
        when (ret /= 1) (throwIO (EmitEventException e ret callStack))
  where
    tagsImplicit (EventScalar _ _ t _) | tagSuppressed t = 1
    tagsImplicit (EventMappingStart _ t _) | tagSuppressed t = 1
    tagsImplicit (EventSequenceStart _ t _) | tagSuppressed t = 1
    tagsImplicit evt = yamlFormatRenderTags fopts evt

    tagSuppressed (NoTag) = True
    tagSuppressed (UriTag "") = True
    tagSuppressed _ = False

    withTag tag = CB.withCBytesUnsafe (tagToCBytes tag)
    withAnchor anchor = CB.withCBytesUnsafe (CB.fromText anchor) 

-- | Whether a tag should be rendered explicitly in the output or left
-- implicit.
--
type TagRender = CInt
pattern Explicit, Implicit :: TagRender
pattern Explicit = 0
pattern Implicit = 1

-- | A value for 'yamlFormatRenderTags' that renders no
-- collection tags but all scalar tags (unless suppressed with styles
-- 'NoTag or 'PlainNoTag').
--
renderScalarTags :: Event -> TagRender
renderScalarTags (EventScalar _ _ _ _) = Explicit
renderScalarTags (EventSequenceStart _ _ _) = Implicit
renderScalarTags (EventMappingStart _ _ _) = Implicit
renderScalarTags _ = Implicit

-- | A value for 'yamlFormatRenderTags' that renders all
-- tags (except 'NoTag' tag and 'PlainNoTag' style).
--
renderAllTags :: Event -> TagRender
renderAllTags _ = Explicit

-- | A value for 'yamlFormatRenderTags' that renders no
-- tags.
--
renderNoTags :: Event -> TagRender
renderNoTags _ = Implicit

-- which are instances of 'UriTag'
--
renderUriTags :: Event -> TagRender
renderUriTags (EventScalar _ _ UriTag{} _) = Explicit
renderUriTags (EventSequenceStart _ UriTag{} _) = Explicit
renderUriTags (EventMappingStart _ UriTag{} _) = Explicit
renderUriTags _ = Implicit

-- | Contains options relating to the formatting (indendation, width) of the YAML output.
--
data YAMLFormatOpts = YAMLFormatOpts
    { yamlFormatCanonical  :: Bool     -- ^ use canonical style, default 'False'
    , yamlFormatIndent     :: Int      -- ^ default 4
    , yamlFormatWidth      :: Int      -- ^ default 80
    , yamlFormatRenderTags :: Event -> TagRender
    }

defaultYAMLFormatOpts :: YAMLFormatOpts
defaultYAMLFormatOpts = YAMLFormatOpts False 4 80 renderScalarTags

--------------------------------------------------------------------------------

data File
#ifdef WINDOWS
foreign import ccall unsafe "_fdopen"
#else
foreign import ccall unsafe "fdopen"
#endif
    fdopen :: CInt -> BA## Word8 -> IO (Ptr File)

foreign import ccall unsafe "fclose" fclose :: Ptr File -> IO ()
