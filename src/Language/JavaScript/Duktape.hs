{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- |
-- Module     : Language.JavaScript.Duktape
-- Maintainer : Blake Rain <blake.rain@inchora.com>
--
-- Higher-level binding to Duktape library.
--

module Language.JavaScript.Duktape
  ( -- * Script Heaps
    ScriptContext
  , newContext
  , withDukContext
    -- * Types of Stack Values
  , StackIndex
  , Type (..)
  , checkStackType
  , checkStackTypes
  , getType
  , instanceOf
  , isArray
  , isBoolean
  , isBoundFunction
  , isCFunction
  , isScriptFunction
  , isCallable
  , isError
  , isEvalError
  , isFunction
  , isNaN
  , isNull
  , isNullOrUndefined
  , isNumber
  , isObject
  , isString
  , isUndefined
    -- * Stack Operations
  , pop
  , popn
  , dup
  , dupTop
  , equals
  , getTop
  , getTopIndex
  , insertAt
  , isValidIndex
  , remove
  , replace
  , swap
  , swapTop
  , normalizeIndex
    -- ** Pushing Values
  , pushArray
  , pushBoolean
  , pushInt
  , pushNaN
  , pushNull
  , pushDouble
  , pushObject
  , pushString
  , pushText
  , pushThis
  , pushUndefined
  , pushValue
  , pushJson
  , ErrType (..)
  , FuncRet (..)
  , pushFunction
  , pushCurrentFunction
  , pushGlobalObject
  , pushBuffer
    -- ** Popping Values
  , getBoolean
  , getInt
  , getWord
  , getLength
  , getDouble
  , getString
  , getText
  , toString
  , toText
  , safeToString
  , getValue
  , getJson
  , getBuffer
  , getBufferData
    -- * Object, Array Operations and Enumeration
  , EnumMode (..)
  , enum
  , next
  , enumerate
  , enumerate_
  , join
  , jsonDecode
  , jsonEncode
  , jsonEncode_
  , new
  , call
  , callMethod
  , callProperty
    -- ** Object Properties
  , PropInfo (..)
  , defineProperty
  , deleteProperty
  , deletePropertyByName
  , deletePropertyByIndex
  , getProperty
  , getPropertyByName
  , getPropertyByIndex
  , hasProperty
  , hasPropertyByName
  , hasPropertyByIndex
  , setProperty
  , setPropertyByName
  , setPropertyByIndex
    -- * Compilation
  , CompFlag (..)
  , compile
  , eval
  , eval_
  , evalEither
    -- * Misc.
  , getGlobalString
  , isConstructorCall
  , dumpFunction
  , loadFunction
  ) where

import           Control.Monad hiding (join)

import           Data.Aeson
import           Data.Bits
import           Data.ByteString (ByteString, useAsCStringLen)
import           Data.ByteString.Internal (memcpy)
import           Data.ByteString.Unsafe (unsafePackCStringLen)
import           Data.Default
import           Data.Maybe
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import           Data.Word

import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Marshal (alloca)
import           Foreign.Ptr
import           Foreign.Storable

import           Prelude hiding (isNaN)

import           Language.JavaScript.Duktape.Raw

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AM
#else
import qualified Data.HashMap.Strict as AM
#endif

----------------------------------------------------------------------------------------------------

data ScriptContext =
  ScriptContext (ForeignPtr DukContext)

newContext :: IO (Maybe ScriptContext)
newContext = do
  ptr <- duk_create_heap_default
  if ptr /= nullPtr
     then newForeignPtr duk_destroy_heap_p ptr >>= return . Just . ScriptContext
     else return Nothing

withDukContext :: ScriptContext -> (Ptr DukContext -> IO a) -> IO a
withDukContext (ScriptContext mfpctx) action =
  withForeignPtr mfpctx action

----------------------------------------------------------------------------------------------------

type StackIndex = Int

data Type
  = NoneT
  | UndefinedT
  | NullT
  | BooleanT
  | NumberT
  | StringT
  | ObjectT
  | BufferT
  | PointerT
  | LightFuncT
  deriving (Eq, Show)

typeToDuk :: Type -> DukInt
typeToDuk NoneT      = c_DUK_TYPE_NONE
typeToDuk UndefinedT = c_DUK_TYPE_UNDEFINED
typeToDuk NullT      = c_DUK_TYPE_NULL
typeToDuk BooleanT   = c_DUK_TYPE_BOOLEAN
typeToDuk NumberT    = c_DUK_TYPE_NUMBER
typeToDuk StringT    = c_DUK_TYPE_STRING
typeToDuk ObjectT    = c_DUK_TYPE_OBJECT
typeToDuk BufferT    = c_DUK_TYPE_BUFFER
typeToDuk PointerT   = c_DUK_TYPE_POINTER
typeToDuk LightFuncT = c_DUK_TYPE_LIGHTFUNC

dukToType :: DukInt -> Maybe Type
dukToType n
  | n == c_DUK_TYPE_NONE      = Just NoneT
  | n == c_DUK_TYPE_UNDEFINED = Just UndefinedT
  | n == c_DUK_TYPE_NULL      = Just NullT
  | n == c_DUK_TYPE_BOOLEAN   = Just BooleanT
  | n == c_DUK_TYPE_NUMBER    = Just NumberT
  | n == c_DUK_TYPE_STRING    = Just StringT
  | n == c_DUK_TYPE_OBJECT    = Just ObjectT
  | n == c_DUK_TYPE_BUFFER    = Just BufferT
  | n == c_DUK_TYPE_POINTER   = Just PointerT
  | n == c_DUK_TYPE_LIGHTFUNC = Just LightFuncT
  | otherwise                 = Nothing

typeToDukMask :: Type -> DukUInt
typeToDukMask NoneT      = c_DUK_TYPE_MASK_NONE
typeToDukMask UndefinedT = c_DUK_TYPE_MASK_UNDEFINED
typeToDukMask NullT      = c_DUK_TYPE_MASK_NULL
typeToDukMask BooleanT   = c_DUK_TYPE_MASK_BOOLEAN
typeToDukMask NumberT    = c_DUK_TYPE_MASK_NUMBER
typeToDukMask StringT    = c_DUK_TYPE_MASK_STRING
typeToDukMask ObjectT    = c_DUK_TYPE_MASK_OBJECT
typeToDukMask BufferT    = c_DUK_TYPE_MASK_BUFFER
typeToDukMask PointerT   = c_DUK_TYPE_MASK_POINTER
typeToDukMask LightFuncT = c_DUK_TYPE_MASK_LIGHTFUNC

checkStackType :: ScriptContext -> StackIndex -> Type -> IO Bool
checkStackType sctx idx typ = withDukContext sctx $ \ctx -> do
  res <- duk_check_type ctx (fromIntegral idx) (typeToDuk typ)
  return (res /= 0)

checkStackTypes :: Foldable f => ScriptContext -> StackIndex -> f Type -> IO Bool
checkStackTypes sctx idx typs = withDukContext sctx $ \ctx -> do
  res <- duk_check_type_mask ctx (fromIntegral idx) mask
  return (res /= 0)
  where
    mask = foldl (\m t -> m .|. typeToDukMask t) 0 typs

getType :: ScriptContext -> StackIndex -> IO Type
getType sctx idx = withDukContext sctx $ \ctx ->
  (fromMaybe NoneT . dukToType) <$> duk_get_type ctx (fromIntegral idx)

instanceOf :: ScriptContext -> StackIndex -> StackIndex -> IO Bool
instanceOf sctx a b = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_instanceof ctx (fromIntegral a) (fromIntegral b)

isArray :: ScriptContext -> StackIndex -> IO Bool
isArray sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_array ctx (fromIntegral idx)

isBoolean :: ScriptContext -> StackIndex -> IO Bool
isBoolean sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_boolean ctx (fromIntegral idx)

isBoundFunction :: ScriptContext -> StackIndex -> IO Bool
isBoundFunction sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_bound_function ctx (fromIntegral idx)

isCFunction :: ScriptContext -> StackIndex -> IO Bool
isCFunction sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_c_function ctx (fromIntegral idx)

isScriptFunction :: ScriptContext -> StackIndex -> IO Bool
isScriptFunction sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_ecmascript_function ctx (fromIntegral idx)

isCallable :: ScriptContext -> StackIndex -> IO Bool
isCallable sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_callable ctx (fromIntegral idx)

isError :: ScriptContext -> StackIndex -> IO Bool
isError sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_error ctx (fromIntegral idx)

isEvalError :: ScriptContext -> StackIndex -> IO Bool
isEvalError sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_eval_error ctx (fromIntegral idx)

isFunction :: ScriptContext -> StackIndex -> IO Bool
isFunction sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_function ctx (fromIntegral idx)

isNaN :: ScriptContext -> StackIndex -> IO Bool
isNaN sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_nan ctx (fromIntegral idx)

isNull :: ScriptContext -> StackIndex -> IO Bool
isNull sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_null ctx (fromIntegral idx)

isNullOrUndefined :: ScriptContext -> StackIndex -> IO Bool
isNullOrUndefined sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_null_or_undefined ctx (fromIntegral idx)

isNumber :: ScriptContext -> StackIndex -> IO Bool
isNumber sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_number ctx (fromIntegral idx)

isObject :: ScriptContext -> StackIndex -> IO Bool
isObject sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_object ctx (fromIntegral idx)

isString :: ScriptContext -> StackIndex -> IO Bool
isString sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_string ctx (fromIntegral idx)

isUndefined :: ScriptContext -> StackIndex -> IO Bool
isUndefined sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_undefined ctx (fromIntegral idx)

----------------------------------------------------------------------------------------------------

pop :: ScriptContext -> IO ()
pop sctx = withDukContext sctx duk_pop

popn :: ScriptContext -> Int -> IO ()
popn sctx n = withDukContext sctx $ \ctx ->
  duk_pop_n ctx (fromIntegral n)

dup :: ScriptContext -> StackIndex -> IO ()
dup sctx idx = withDukContext sctx $ \ctx ->
  duk_dup ctx (fromIntegral idx)

dupTop :: ScriptContext -> IO ()
dupTop sctx = withDukContext sctx duk_dup_top

equals :: ScriptContext -> StackIndex -> StackIndex -> IO Bool
equals sctx a b = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_equals ctx (fromIntegral a) (fromIntegral b)

getTop :: ScriptContext -> IO StackIndex
getTop sctx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_get_top ctx

getTopIndex :: ScriptContext -> IO StackIndex
getTopIndex sctx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_get_top_index ctx

insertAt :: ScriptContext -> StackIndex -> IO ()
insertAt sctx idx = withDukContext sctx $ \ctx ->
  duk_insert ctx (fromIntegral idx)

isValidIndex :: ScriptContext -> StackIndex -> IO Bool
isValidIndex sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_valid_index ctx (fromIntegral idx)

remove :: ScriptContext -> StackIndex -> IO ()
remove sctx idx = withDukContext sctx $ \ctx ->
  duk_remove ctx (fromIntegral idx)

replace :: ScriptContext -> StackIndex -> IO ()
replace sctx idx = withDukContext sctx $ \ctx ->
  duk_replace ctx (fromIntegral idx)

swap :: ScriptContext -> StackIndex -> StackIndex -> IO ()
swap sctx from to = withDukContext sctx $ \ctx ->
  duk_swap ctx (fromIntegral from) (fromIntegral to)

swapTop :: ScriptContext -> StackIndex -> IO ()
swapTop sctx from = withDukContext sctx $ \ctx ->
  duk_swap_top ctx (fromIntegral from)

normalizeIndex :: ScriptContext -> StackIndex -> IO StackIndex
normalizeIndex sctx idx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_normalize_index ctx (fromIntegral idx)

----------------------------------------------------------------------------------------------------

-- | Push an empty array object onto the stack.
--
-- The function returns the stack index of the new array.
pushArray :: ScriptContext -> IO StackIndex
pushArray sctx = withDukContext sctx $ \ctx -> fromIntegral <$> duk_push_array ctx

-- | Push a boolean value onto the stack.
pushBoolean :: ScriptContext -> Bool -> IO ()
pushBoolean sctx val = withDukContext sctx $ \ctx ->
  duk_push_boolean ctx (if val then 1 else 0)

-- | Push an integer value onto the stack.
pushInt :: ScriptContext -> Int -> IO ()
pushInt sctx val = withDukContext sctx $ \cxt -> duk_push_int cxt (fromIntegral val)

-- | Push a @NaN@ onto the stack.
pushNaN :: ScriptContext -> IO ()
pushNaN sctx = withDukContext sctx duk_push_nan

-- | Push a @null@ onto the stack.
pushNull :: ScriptContext -> IO ()
pushNull sctx = withDukContext sctx duk_push_null

-- | Push a 'Double' onto the stack.
pushDouble :: ScriptContext -> Double -> IO ()
pushDouble sctx val = withDukContext sctx $ \cxt -> duk_push_number cxt (CDouble val)

-- | Push an empty object onto the stack.
--
-- The function returns the stack index of the new object.
pushObject :: ScriptContext -> IO StackIndex
pushObject sctx = withDukContext sctx $ \cxt -> fromIntegral <$> duk_push_object cxt

-- | Push a string onto the stack.
pushString :: ScriptContext -> String -> IO ()
pushString sctx str = withDukContext sctx $ \cxt -> withCString str $ \cstr ->
  void (duk_push_string cxt cstr)

-- | Push the current @this@ onto the stack.
pushThis :: ScriptContext -> IO ()
pushThis sctx = withDukContext sctx duk_push_this

-- | Push the @undefined@ onto the stack.
pushUndefined :: ScriptContext -> IO ()
pushUndefined sctx = withDukContext sctx duk_push_undefined

pushText :: ScriptContext -> Text -> IO ()
pushText cxt str = pushString cxt (unpack str)

pushValue :: ScriptContext -> Value -> IO ()
pushValue cxt val = case val of
  Null     -> pushNull cxt
  Bool   b -> pushBoolean cxt b
  Number s -> pushDouble cxt (fromRational (toRational s))
  String s -> pushText cxt s
  Array  a -> do
    ix <- pushArray cxt
    let f i v = do
          pushValue cxt v
          setPropertyByIndex cxt ix (fromIntegral i)
    V.imapM_ f a
  Object o -> do
    ix <- pushObject cxt
    let f (k, v) = do
          pushValue cxt v
          setPropertyByName cxt ix (unpack k)
    mapM_ f (aesonObjectToList o)

pushJson :: ToJSON a => ScriptContext -> a -> IO ()
pushJson cxt val = pushValue cxt (toJSON val)

-- | Error types
data ErrType
  = ErrError
  | ErrEvalError
  | ErrRangeError
  | ErrReferenceError
  | ErrSyntaxError
  | ErrTypeError
  | ErrUriError
  deriving (Eq, Show)

fromErrType :: ErrType -> CInt
fromErrType ErrError              = c_DUK_ERR_ERROR
fromErrType ErrEvalError          = c_DUK_ERR_EVAL_ERROR
fromErrType ErrRangeError         = c_DUK_ERR_RANGE_ERROR
fromErrType ErrReferenceError     = c_DUK_ERR_REFERENCE_ERROR
fromErrType ErrSyntaxError        = c_DUK_ERR_SYNTAX_ERROR
fromErrType ErrTypeError          = c_DUK_ERR_TYPE_ERROR
fromErrType ErrUriError           = c_DUK_ERR_URI_ERROR

-- pushErrorObject :: ScriptContext -> ErrType -> String -> IO StackIndex
-- pushErrorObject sctx etype message = withDukContext sctx $ \ctx ->
--   withCString message $ \cmessage ->
--     fromIntegral <$> duk_push_error_object ctx (fromErrType etype) cmessage

-- scriptThrow :: ScriptContext -> IO ()
-- scriptThrow sctx = withDukContext sctx duk_throw

-- | Haskell function return value.
data FuncRet
  = RetUndefined            -- ^ The function has no return value
  | RetTop                  -- ^ The return value is on the top of the stack
  | RetError ErrType        -- ^ There was an error
  deriving (Eq, Show)

fromFuncRet :: FuncRet -> DukRet
fromFuncRet RetUndefined = 0
fromFuncRet RetTop       = 1
fromFuncRet (RetError e) = negate (fromErrType e)

-- | Push a new function object, associated with a Haskell function, onto the stack.
--
-- @
-- let myAddTwo = do
--       a <- getDouble 0
--       b <- getDouble 1
--       pushDouble (a + b)
--       return RetTop
--
-- fnidx <- pushFunction 2 myAddTwo
-- pushInt 2
-- pushInt 3
-- call 2
-- r <- getDouble
-- liftIO $ putStrLn ("Result: " ++ show r)
-- @
--
-- Outputs:
--
-- @
-- Result: 5.0
-- @
--
pushFunction :: ScriptContext -> Int -> IO FuncRet -> IO StackIndex
pushFunction sctx nargs func = withDukContext sctx $ \ctx -> do
  wrapped <- c_wrapper (fmap fromFuncRet . const func)
  fromIntegral <$> duk_push_c_function ctx wrapped (fromIntegral nargs)

foreign import ccall safe "wrapper"
  c_wrapper :: DukCFunction -> IO (FunPtr DukCFunction)

-- | Push the current function object onto the stack.
pushCurrentFunction :: ScriptContext -> IO ()
pushCurrentFunction sctx = withDukContext sctx duk_push_current_function

-- | Push the global object onto the stack.
pushGlobalObject :: ScriptContext -> IO ()
pushGlobalObject sctx = withDukContext sctx duk_push_global_object

pushBuffer :: ScriptContext -> ByteString -> IO ()
pushBuffer sctx bs = withDukContext sctx $ \ctx ->
  useAsCStringLen bs $ \(pbuf, len) -> do
    ptr <- duk_push_fixed_buffer ctx (fromIntegral len)
    memcpy ptr (castPtr pbuf) len

----------------------------------------------------------------------------------------------------

-- | Read a boolean value from the stack.
getBoolean :: ScriptContext -> StackIndex -> IO Bool
getBoolean sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_get_boolean ctx (fromIntegral idx)

getInt :: ScriptContext -> StackIndex -> IO Int
getInt sctx idx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_get_int ctx (fromIntegral idx)

getWord :: ScriptContext -> StackIndex -> IO Word
getWord sctx idx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_get_uint ctx (fromIntegral idx)

getLength :: ScriptContext -> StackIndex -> IO Int
getLength sctx idx = withDukContext sctx $ \ctx ->
  fromIntegral <$> duk_get_length ctx (fromIntegral idx)

getDouble :: ScriptContext -> StackIndex -> IO Double
getDouble sctx idx = withDukContext sctx $ \ctx -> do
  CDouble val <- duk_get_number ctx (fromIntegral idx)
  return val

getString :: ScriptContext -> StackIndex -> IO String
getString sctx idx = withDukContext sctx $ \ctx -> do
  cstr <- duk_get_string ctx (fromIntegral idx)
  peekCString cstr

getText :: ScriptContext -> StackIndex -> IO Text
getText sctx idx = pack <$> getString sctx idx

toString :: ScriptContext -> StackIndex -> IO String
toString sctx idx = withDukContext sctx $ \ctx -> do
  cstr <- duk_to_string ctx (fromIntegral idx)
  peekCString cstr

toText :: ScriptContext -> StackIndex -> IO Text
toText sctx idx = pack <$> toString sctx idx

safeToString :: ScriptContext -> StackIndex -> IO String
safeToString sctx idx = withDukContext sctx $ \ctx ->
  peekCString =<< duk_safe_to_string ctx (fromIntegral idx)

getValue :: ScriptContext -> StackIndex -> IO (Maybe Value)
getValue sctx idx' = do
  idx   <- normalizeIndex sctx idx'
  stype <- getType sctx idx
  case stype of
    NullT      -> return (Just Null)
    BooleanT   -> Just . Bool    <$> getBoolean sctx idx
    StringT    -> Just . String  <$> getText sctx idx
    NumberT    ->
      Just . Number . fromRational . toRational <$> getDouble sctx idx
    ObjectT    -> do
      arr <- isArray sctx idx
      if arr
         then Just . Array . V.fromList . catMaybes <$>
                (enumerate sctx idx (EnumArrayIndices True) True $ const $
                   getValue sctx (negate 1))
         else do
           has_date <- getGlobalString sctx "Date"
           if has_date
              then do
                is_date <- instanceOf sctx idx (negate 1)
                pop sctx
                if is_date
                   then either (const Nothing) Just <$> getJsonEncoded sctx idx
                   else objectFallback idx
              else do
                pop sctx
                objectFallback idx
    _          -> return Nothing
  where
    objectFallback idx =
      Just . jsonObjectFromList . catMaybes <$>
        (enumerate sctx idx (EnumProperties True False False) True $ const $ do
            key  <- getText sctx (negate 2)
            fmap (key, ) <$> getValue sctx (negate 1))

jsonObjectFromList :: [(Text, Value)] -> Value
jsonObjectFromList vals =
  Object $ AM.fromList $ do
    (k, v) <- vals
    pure (textToAesonKey k, v)

#if MIN_VERSION_aeson(2,0,0)
textToAesonKey :: Text -> Key
textToAesonKey = AK.fromText
#else
textToAesonKey :: Text -> Text
textToAesonKey = id
#endif

#if MIN_VERSION_aeson(2,0,0)
aesonObjectToList :: AM.KeyMap Value -> [(Text, Value)]
aesonObjectToList km = do
  (k, v) <- AM.toList km
  pure (AK.toText k, v)
#else
aesonObjectToList :: Object -> [(Text, Value)]
aesonObjectToList =
  AM.toList
#endif

--getValue :: ScriptContext -> StackIndex -> IO (Either String Value)
--getValue = getJson

getJson :: FromJSON a => ScriptContext -> StackIndex -> IO (Either String a)
getJson sctx idx = do
  mval <- getValue sctx idx
  case mval of
    Nothing -> do
      itype <- getType sctx idx
      return (Left ("Could not build JSON value for: " ++ show itype))
    Just val -> case fromJSON val of
      Error err -> return (Left err)
      Success r -> return (Right r)

getJsonEncoded :: FromJSON a => ScriptContext -> StackIndex -> IO (Either String a)
getJsonEncoded sctx idx = withDukContext sctx $ \ctx -> do
  duk_dup ctx (fromIntegral idx)
  cstr <- duk_json_encode ctx (negate 1)
  duk_pop ctx
  str <- peekCString cstr
  return (eitherDecodeStrict (encodeUtf8 (pack str)))

--getJson :: FromJSON a => ScriptContext -> StackIndex -> IO (Either String a)
--getJson sctx idx = withDukContext sctx $ \ctx -> do
--  duk_dup ctx (fromIntegral idx)
--  cstr <- duk_json_encode ctx (negate 1)
--  duk_pop ctx
--  str <- peekCString cstr
--  putStrLn ("Encoded JSON: " ++ str)
--  return (eitherDecodeStrict (encodeUtf8 (pack str)))

getBuffer :: ScriptContext -> StackIndex -> IO ByteString
getBuffer sctx idx = withDukContext sctx $ \ctx -> do
  (pbuf, len) <- alloca $ \plen -> do
    pbuf <- duk_get_buffer ctx (fromIntegral idx) plen
    (,) pbuf <$> peek plen
  unsafePackCStringLen (pbuf, fromIntegral len)

getBufferData :: ScriptContext -> StackIndex -> IO ByteString
getBufferData sctx idx = withDukContext sctx $ \ctx -> do
  (pbuf, len) <- alloca $ \plen -> do
    pbuf <- duk_get_buffer_data ctx (fromIntegral idx) plen
    (,) pbuf <$> peek plen
  unsafePackCStringLen (pbuf, fromIntegral len)

----------------------------------------------------------------------------------------------------

getGlobalString :: ScriptContext -> String -> IO Bool
getGlobalString sctx name = withDukContext sctx $ \ctx -> withCString name $ \cname ->
  (/= 0) <$> duk_get_global_string ctx cname

isConstructorCall :: ScriptContext -> IO Bool
isConstructorCall sctx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_is_constructor_call ctx

dumpFunction :: ScriptContext -> IO ()
dumpFunction sctx = withDukContext sctx $ \ctx ->
  duk_dump_function ctx

loadFunction :: ScriptContext -> IO ()
loadFunction sctx = withDukContext sctx $ \ctx ->
  duk_load_function ctx

----------------------------------------------------------------------------------------------------

data EnumMode
  = EnumArrayIndices { enumSortArrayIndices :: Bool
                     }
  | EnumProperties { enumOwnPropertiesOnly    :: Bool
                   , enumIncludeInternal      :: Bool
                   , enumIncludeNonEnumerable :: Bool
                   }
  deriving (Show)

enumModeFlags :: EnumMode -> DukUInt
enumModeFlags (EnumArrayIndices sorted) =
  c_DUK_ENUM_ARRAY_INDICES_ONLY .|. (if sorted then c_DUK_ENUM_SORT_ARRAY_INDICES else 0)
enumModeFlags (EnumProperties own internal nonenum) =
  (if own      then c_DUK_ENUM_OWN_PROPERTIES_ONLY   else 0) .|.
  (if internal then c_DUK_ENUM_INCLUDE_HIDDEN        else 0) .|.
  (if nonenum  then c_DUK_ENUM_INCLUDE_NONENUMERABLE else 0)

enum :: ScriptContext ->StackIndex -> EnumMode -> IO ()
enum sctx idx mode = withDukContext sctx $ \ctx ->
  duk_enum ctx (fromIntegral idx) (enumModeFlags mode)

next :: ScriptContext -> StackIndex -> Bool -> IO Bool
next sctx idx getval = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_next ctx (fromIntegral idx) (if getval then 1 else 0)

enumerate :: ScriptContext -> StackIndex -> EnumMode -> Bool -> (ScriptContext -> IO a) -> IO [a]
enumerate ctx idx mode getval action = do
  enum ctx idx mode
  rs <- go id
  pop ctx
  return rs
  where
    go f = do
      present <- next ctx (negate 1) getval
      if present
        then do
          r <- action ctx
          when getval (pop ctx)
          pop ctx
          go (f . (r :))
        else return (f [])

enumerate_ :: ScriptContext -> StackIndex -> EnumMode -> Bool -> (ScriptContext -> IO a) -> IO ()
enumerate_ ctx idx mode getval action = do
  enum ctx idx mode
  go
  pop ctx
  where
    go = do
      present <- next ctx (negate 1) getval
      when present $ do
        void (action ctx)
        when getval (pop ctx)
        pop ctx
        go

-- |  Join zero  or more  values into  a  result string  with a  separator between  each value.  The
-- separator  and  the  input values  are  automatically  coerced  to  strings with  the  ECMAScript
-- @ToString@.
--
-- @
-- pushString "; "          -- [ '; '                 ]
-- pushString "foo"         -- [ '; '  foo            ]
-- pushInt 2                -- [ '; '  foo  2         ]
-- pushString "bar"         -- [ '; '  foo  2  bar    ]
-- pushInt 3                -- [ '; '  foo  2  bar  3 ]
-- join 4                   -- [ 'foo; 2; bar; 3'     ]
-- @
--
join :: ScriptContext -> Int -> IO ()
join sctx n = withDukContext sctx $ \cxt ->
  duk_join cxt (fromIntegral n)

-- | Decodes a JSON value as an in-place operation. If the input is invalid, throws an error.
--
-- @
-- pushString "{ a: 1, b: 2 }"          -- [ "{ a: 1, b: 2 }" ]
-- jsonDecode (negate 1)                -- [  { a: 1, b: 2 }  ]
-- @
--
jsonDecode :: ScriptContext -> StackIndex -> IO ()
jsonDecode sctx idx = withDukContext sctx $ \cxt ->
  duk_json_decode cxt (fromIntegral idx)

-- | Encode value to JSON string in-place. Returns a string copy of the encoded value.
--
-- @
-- pushObject
-- pushInt 42
-- setPropertyByName (negate 2) "meaningOfLife"
-- result <- jsonEncode (negate 1)
-- liftIO $ putStrLn result
-- @
--
-- Will output the string: @{"meaningOfLife":42}@
--
jsonEncode :: ScriptContext -> StackIndex -> IO String
jsonEncode sctx idx = withDukContext sctx $ \cxt -> do
  cstr <- duk_json_encode cxt (fromIntegral idx)
  peekCString cstr

-- | Same as 'jsonEncode', except does not return a copy of the encoded string.
--
jsonEncode_ :: ScriptContext -> StackIndex -> IO ()
jsonEncode_ sctx idx = withDukContext sctx $ \cxt ->
  void (duk_json_encode cxt (fromIntegral idx))

-- | Call  a constructor function with  a specified number  of arguments (not counting  the function
-- itself). The  function and  its arguments  are replaced by  a single  return value.  The function
-- returns @Nothing@ if there was no error.
--
-- @
-- {- Protected call to: new MyConstructor ("foo", 123) -}
-- eval "MyConstructor"
-- pushString "foo"
-- pushInt 123
-- merr <- new 2
-- case merr of
--   Nothing -> liftIO (putStrLn "Success: instance is on the stack")
--   Just (rc, str) -> do
--     {- Discard error object and report failure string -}
--     pop
--     liftIO $ do
--       putStrLn ("Failure: error code " ++ show rc)
--       putStrLn ("         " ++ str)
-- @
--
new :: ScriptContext -> Int -> IO (Maybe (Int, String))
new sctx n = withDukContext sctx $ \cxt -> do
  rc <- duk_pnew cxt (fromIntegral n)
  if rc /= 0
    then do
      res <- duk_safe_to_string cxt (negate 1)
      Just . (fromIntegral rc, ) <$> peekCString res
    else return Nothing

call :: ScriptContext -> Int -> IO Bool
call sctx nargs = withDukContext sctx $ \ctx -> do
  rc <- duk_pcall ctx (fromIntegral nargs)
  return (rc == c_DUK_EXEC_SUCCESS)

callMethod :: ScriptContext -> Int -> IO Bool
callMethod sctx nargs = withDukContext sctx $ \ctx -> do
  rc <- duk_pcall_method ctx (fromIntegral nargs)
  return (rc == c_DUK_EXEC_SUCCESS)

callProperty :: ScriptContext -> StackIndex -> Int -> IO Bool
callProperty sctx idx nargs = withDukContext sctx $ \ctx -> do
  rc <- duk_pcall_prop ctx (fromIntegral idx) (fromIntegral nargs)
  return (rc == c_DUK_EXEC_SUCCESS)

----------------------------------------------------------------------------------------------------

data CompFlag
  = CompEval
  | CompFunction
  | CompStrict
  deriving (Eq, Show)

compFlagToOpt :: CompFlag -> DukUInt
compFlagToOpt CompEval     = c_DUK_COMPILE_EVAL
compFlagToOpt CompFunction = c_DUK_COMPILE_FUNCTION
compFlagToOpt CompStrict   = c_DUK_COMPILE_STRICT

compile :: ScriptContext -> Maybe FilePath -> String -> [CompFlag] -> IO (Maybe (Int, String))
compile sctx mfilename src flags = withDukContext sctx $ \cxt ->
  withCString src $ \csrc -> do
    rc <- action cxt csrc
    if rc /= 0
      then do
        str <- duk_safe_to_string cxt (negate 1)
        Just . (fromIntegral rc, ) <$> peekCString str
      else return Nothing
    where
      flag = foldl (\m f -> m .|. compFlagToOpt f) 0 flags
      action cxt str = case mfilename of
        Just filename -> do
          withCString filename (void . duk_push_string cxt)
          duk_pcompile_string_filename cxt flag str
        Nothing -> duk_pcompile_string cxt flag str

eval :: ScriptContext -> String -> IO (Maybe (Int, String))
eval sctx src = withDukContext sctx $ \ctx -> withCString src $ \csrc -> do
  rc <- duk_peval_string ctx csrc
  if rc /= 0
    then do
      str <- duk_safe_to_string ctx (negate 1)
      Just . (fromIntegral rc, ) <$> peekCString str
    else return Nothing

eval_ :: ScriptContext -> String -> IO Bool
eval_ sctx src = withDukContext sctx $ \ctx -> withCString src $ \csrc ->
  (== 0) <$> duk_peval_string_noresult ctx csrc

evalEither :: FromJSON a => ScriptContext -> String -> IO (Either (Int, String) a)
evalEither scxt src = do
  merr <- eval scxt src
  case merr of
    Just err -> return (Left err)
    Nothing  -> do
      eres <- getJson scxt 0
      pop scxt
      case eres of
        Left  err -> return (Left (negate 1, err))
        Right res -> return (Right res)

----------------------------------------------------------------------------------------------------

data PropInfo =
  PropInfo { propInfoHaveValue      :: Bool
           , propInfoHaveGetter     :: Bool
           , propInfoHaveSetter     :: Bool
           , propInfoIsWritable     :: Bool
           , propInfoIsEnumerable   :: Bool
           , propInfoIsConfigurable :: Bool
           }
  deriving (Show)

instance Default PropInfo where
  def = PropInfo True True True True True True

defineProperty :: ScriptContext -> StackIndex -> PropInfo -> IO ()
defineProperty sctx idx PropInfo{..} = withDukContext sctx $ \ctx -> do
  duk_def_prop ctx (fromIntegral idx) flags
  where
    flags = (if propInfoHaveValue  then c_DUK_DEFPROP_HAVE_VALUE  else 0) .|.
            (if propInfoHaveGetter then c_DUK_DEFPROP_HAVE_GETTER else 0) .|.
            (if propInfoHaveSetter then c_DUK_DEFPROP_HAVE_SETTER else 0) .|.
            (if not (propInfoHaveGetter || propInfoHaveSetter)
                then if propInfoIsWritable
                        then c_DUK_DEFPROP_SET_WRITABLE else c_DUK_DEFPROP_CLEAR_WRITABLE
                else 0) .|.
            (if propInfoIsEnumerable
                then c_DUK_DEFPROP_SET_ENUMERABLE else c_DUK_DEFPROP_CLEAR_ENUMERABLE) .|.
            (if propInfoIsConfigurable
                then c_DUK_DEFPROP_SET_CONFIGURABLE else c_DUK_DEFPROP_CLEAR_CONFIGURABLE)

deleteProperty :: ScriptContext -> StackIndex -> IO Bool
deleteProperty sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_del_prop ctx (fromIntegral idx)

deletePropertyByName :: ScriptContext -> StackIndex -> String -> IO Bool
deletePropertyByName sctx idx name = withDukContext sctx $ \ctx ->
  withCString name $ \cname -> (/= 0) <$> duk_del_prop_string ctx (fromIntegral idx) cname

deletePropertyByIndex :: ScriptContext -> StackIndex -> Word32 -> IO Bool
deletePropertyByIndex sctx idx i = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_del_prop_index ctx (fromIntegral idx) (fromIntegral i)

getProperty :: ScriptContext -> StackIndex -> IO Bool
getProperty sctx idx =
  withDukContext sctx $ \ctx -> (/= 0) <$> duk_get_prop ctx (fromIntegral idx)

getPropertyByName :: ScriptContext -> StackIndex -> String -> IO Bool
getPropertyByName sctx idx name = withDukContext sctx $ \ctx ->
  withCString name $ \cname -> (/= 0) <$> duk_get_prop_string ctx (fromIntegral idx) cname

getPropertyByIndex :: ScriptContext -> StackIndex -> Word32 -> IO Bool
getPropertyByIndex sctx idx i = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_get_prop_index ctx (fromIntegral idx) (fromIntegral i)

hasProperty :: ScriptContext -> StackIndex -> IO Bool
hasProperty sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_has_prop ctx (fromIntegral idx)

hasPropertyByName :: ScriptContext -> StackIndex -> String -> IO Bool
hasPropertyByName sctx idx name = withDukContext sctx $ \ctx ->
  withCString name $ \cname -> (/= 0) <$> duk_has_prop_string ctx (fromIntegral idx) cname

hasPropertyByIndex :: ScriptContext -> StackIndex -> Word32 -> IO Bool
hasPropertyByIndex sctx idx i = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_has_prop_index ctx (fromIntegral idx) (fromIntegral i)

setProperty :: ScriptContext -> StackIndex -> IO Bool
setProperty sctx idx = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_put_prop ctx (fromIntegral idx)

setPropertyByName :: ScriptContext -> StackIndex -> String -> IO Bool
setPropertyByName sctx idx name = withDukContext sctx $ \ctx ->
  withCString name $ \cname -> (/= 0) <$> duk_put_prop_string ctx (fromIntegral idx) cname

setPropertyByIndex :: ScriptContext -> StackIndex -> Word32 -> IO Bool
setPropertyByIndex sctx idx i = withDukContext sctx $ \ctx ->
  (/= 0) <$> duk_put_prop_index ctx (fromIntegral idx) (fromIntegral i)
