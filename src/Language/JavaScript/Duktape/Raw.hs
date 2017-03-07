{-# LANGUAGE CPP                      #-}
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ForeignFunctionInterface #-}

--
-- |
-- Module     : Language.JavaScript.Duktape.Raw
-- Maintainer : Blake Rain <blake.rain@inchora.com>
--
-- Raw interface to Duktape JavaScript library.
--

module Language.JavaScript.Duktape.Raw
  (   -- * Stack Types
      c_DUK_TYPE_NONE
    , c_DUK_TYPE_UNDEFINED
    , c_DUK_TYPE_NULL
    , c_DUK_TYPE_BOOLEAN
    , c_DUK_TYPE_NUMBER
    , c_DUK_TYPE_STRING
    , c_DUK_TYPE_OBJECT
    , c_DUK_TYPE_BUFFER
    , c_DUK_TYPE_POINTER
    , c_DUK_TYPE_LIGHTFUNC
      -- * Stack Type Masks
    , c_DUK_TYPE_MASK_NONE
    , c_DUK_TYPE_MASK_UNDEFINED
    , c_DUK_TYPE_MASK_NULL
    , c_DUK_TYPE_MASK_BOOLEAN
    , c_DUK_TYPE_MASK_NUMBER
    , c_DUK_TYPE_MASK_STRING
    , c_DUK_TYPE_MASK_OBJECT
    , c_DUK_TYPE_MASK_BUFFER
    , c_DUK_TYPE_MASK_POINTER
    , c_DUK_TYPE_MASK_LIGHTFUNC
      -- * ECMAScript E5 Errors
    , c_DUK_ERR_NONE
    , c_DUK_ERR_ERROR
    , c_DUK_ERR_EVAL_ERROR
    , c_DUK_ERR_RANGE_ERROR
    , c_DUK_ERR_REFERENCE_ERROR
    , c_DUK_ERR_SYNTAX_ERROR
    , c_DUK_ERR_TYPE_ERROR
    , c_DUK_ERR_URI_ERROR
      -- * Return Codes from Duktape/C Functions
    , c_DUK_RET_ERROR
    , c_DUK_RET_EVAL_ERROR
    , c_DUK_RET_RANGE_ERROR
    , c_DUK_RET_REFERENCE_ERROR
    , c_DUK_RET_SYNTAX_ERROR
    , c_DUK_RET_TYPE_ERROR
    , c_DUK_RET_URI_ERROR
      -- * Return Codes for Protected Calls
    , c_DUK_EXEC_SUCCESS
    , c_DUK_EXEC_ERROR
      -- * Compilation Flags
    , c_DUK_COMPILE_EVAL
    , c_DUK_COMPILE_FUNCTION
    , c_DUK_COMPILE_STRICT
      -- * Property Flags
    , c_DUK_DEFPROP_WRITABLE
    , c_DUK_DEFPROP_ENUMERABLE
    , c_DUK_DEFPROP_CONFIGURABLE
    , c_DUK_DEFPROP_HAVE_WRITABLE
    , c_DUK_DEFPROP_HAVE_ENUMERABLE
    , c_DUK_DEFPROP_HAVE_CONFIGURABLE
    , c_DUK_DEFPROP_HAVE_VALUE
    , c_DUK_DEFPROP_HAVE_GETTER
    , c_DUK_DEFPROP_HAVE_SETTER
    , c_DUK_DEFPROP_FORCE
    , c_DUK_DEFPROP_SET_WRITABLE
    , c_DUK_DEFPROP_CLEAR_WRITABLE
    , c_DUK_DEFPROP_SET_ENUMERABLE
    , c_DUK_DEFPROP_CLEAR_ENUMERABLE
    , c_DUK_DEFPROP_SET_CONFIGURABLE
    , c_DUK_DEFPROP_CLEAR_CONFIGURABLE
      -- * Enumeration Flags
    , c_DUK_ENUM_INCLUDE_NONENUMERABLE
    , c_DUK_ENUM_INCLUDE_HIDDEN
    , c_DUK_ENUM_OWN_PROPERTIES_ONLY
    , c_DUK_ENUM_ARRAY_INDICES_ONLY
    , c_DUK_ENUM_SORT_ARRAY_INDICES
    , c_DUK_ENUM_NO_PROXY_BEHAVIOR
      -- * Coercion Hints
    , c_DUK_HINT_NONE
    , c_DUK_HINT_STRING
    , c_DUK_HINT_NUMBER
      -- * Flags for 'duk_push_thread_raw'
    , c_DUK_THREAD_NEW_GLOBAL_ENV
      -- * Misc Defines
    , c_DUK_INVALID_INDEX
    , c_DUK_VARARGS
    , c_DUK_API_ENTRY_STACK
      -- * Types
    , DukContext
    , DukBool
    , DukInt
    , DukInt32
    , DukUInt
    , DukUInt16
    , DukUInt32
    , DukIdx
    , DukArrIdx
    , DukUArrIdx
    , DukErrCode
    , DukSize
    , DukRet
    , DukDouble
    , DukCodePoint
      -- * Buffers
    , duk_config_buffer
    , duk_push_dynamic_buffer
    , duk_push_external_buffer
    , duk_resize_buffer
    , duk_to_buffer
      -- * Byte Code
    , duk_dump_function
    , duk_load_function
      -- * Call
    , duk_call
    , duk_call_method
    , duk_call_prop
      -- * Safe Call
    , duk_pcall
    , duk_pcall_method
    , duk_pcall_prop
    , duk_safe_call
      -- * Codec
    , duk_base64_decode
    , duk_base64_encode
    , duk_hex_decode
    , duk_hex_encode
    , duk_json_decode
    , duk_json_encode
      -- * Compare
    , duk_equals
    , duk_instanceof
    , duk_strict_equals
      -- * Compile
    , duk_compile
    , duk_compile_lstring
    , duk_compile_lstring_filename
    , duk_compile_string
    , duk_compile_string_filename
    , duk_eval
    , duk_eval_lstring
    , duk_eval_lstring_noresult
    , duk_eval_noresult
    , duk_eval_string
    , duk_eval_string_noresult
      -- ** Safe Compile
    , duk_pcompile
    , duk_pcompile_lstring
    , duk_pcompile_lstring_filename
    , duk_pcompile_string
    , duk_pcompile_string_filename
    , duk_peval
    , duk_peval_lstring
    , duk_peval_lstring_noresult
    , duk_peval_noresult
    , duk_peval_string
    , duk_peval_string_noresult
      -- * Debug
    , DukDebugReadFunction
    , DukDebugWriteFunction
    , DukDebugPeekFunction
    , DukDebugReadFlushFunction
    , DukDebugWriteFlushFunction
    , duk_debugger_attach
    , duk_debugger_cooperate
    , duk_debugger_detach
    , duk_push_context_dump
      -- * Error
    , duk_fatal
    , duk_get_error_code
    , duk_is_error
    , duk_is_eval_error
    , duk_is_range_error
    , duk_is_reference_error
    , duk_is_syntax_error
    , duk_is_type_error
    , duk_is_uri_error
    , duk_throw
      -- * Finalizer
    , duk_get_finalizer
    , duk_set_finalizer
      -- * Functions
    , DukCFunction
    , duk_get_current_magic
    , duk_get_magic
    , duk_is_strict_call
    , duk_push_current_function
    , duk_push_current_thread
    , duk_set_magic
      -- * Heap
    , DukAllocFunction
    , DukReallocFunction
    , DukFreeFunction
    , DukFatalFunction
    , duk_create_heap
    , duk_create_heap_default
    , duk_destroy_heap
    , duk_destroy_heap_p
    , duk_gc
      -- * Memory
    , duk_alloc
    , duk_alloc_raw
    , duk_compact
    , duk_free
    , duk_free_raw
    , duk_realloc
    , duk_realloc_raw
      -- * Module
    , duk_push_global_stash
    , duk_push_heap_stash
    , duk_push_thread_stash
    , duk_get_global_string
      -- * Object
    , duk_get_prototype
    , duk_new
    , duk_pnew
    , duk_push_global_object
    , duk_push_heapptr
    , duk_set_prototype
      -- * Property
    , duk_def_prop
    , duk_del_prop
    , duk_del_prop_index
    , duk_del_prop_string
    , duk_get_prop
    , duk_get_prop_index
    , duk_get_prop_string
    , duk_has_prop
    , duk_has_prop_index
    , duk_has_prop_string
    , duk_enum
    , duk_next
    , duk_put_prop
    , duk_put_prop_index
    , duk_put_prop_string
    , duk_put_global_string
      -- * Stack
    , duk_check_stack
    , duk_check_stack_top
    , duk_check_type
    , duk_check_type_mask
    , duk_copy
    , duk_dup
    , duk_dup_top
    , duk_get_boolean
    , duk_get_buffer
    , duk_get_buffer_data
    , duk_get_c_function
    , duk_get_context
    , duk_get_heapptr
    , duk_get_int
    , duk_get_length
    , duk_get_lstring
    , duk_get_number
    , duk_get_pointer
    , duk_get_string
    , duk_get_top
    , duk_get_top_index
    , duk_get_type
    , duk_get_type_mask
    , duk_get_uint
    , duk_insert
    , duk_is_array
    , duk_is_boolean
    , duk_is_bound_function
    , duk_is_buffer
    , duk_is_c_function
    , duk_is_callable
    , duk_is_constructor_call
    , duk_is_dynamic_buffer
    , duk_is_ecmascript_function
    , duk_is_fixed_buffer
    , duk_is_function
    , duk_is_lightfunc
    , duk_is_nan
    , duk_is_null
    , duk_is_null_or_undefined
    , duk_is_number
    , duk_is_object
    , duk_is_object_coercible
    , duk_is_pointer
    , duk_is_primitive
    , duk_is_string
    , duk_is_thread
    , duk_is_undefined
    , duk_is_valid_index
    , duk_normalize_index
    , duk_pop
    , duk_pop_2
    , duk_pop_3
    , duk_pop_n
    , duk_push_array
    , duk_push_boolean
    , duk_push_buffer
    , duk_push_buffer_object
    , duk_push_c_function
    , duk_push_c_lightfunc
    , duk_push_error_object
    , duk_push_false
    , duk_push_fixed_buffer
    , duk_push_int
    , duk_push_lstring
    , duk_push_nan
    , duk_push_null
    , duk_push_number
    , duk_push_object
    , duk_push_pointer
    , duk_push_string
    , duk_push_this
    , duk_push_thread
    , duk_push_thread_new_globalenv
    , duk_push_true
    , duk_push_uint
    , duk_push_undefined
    , duk_remove
    , duk_replace
    , duk_require_boolean
    , duk_require_buffer
    , duk_require_buffer_data
    , duk_require_c_function
    , duk_require_callable
    , duk_require_context
    , duk_require_function
    , duk_require_heapptr
    , duk_require_int
    , duk_require_lstring
    , duk_require_normalize_index
    , duk_require_null
    , duk_require_number
    , duk_require_object_coercible
    , duk_require_pointer
    , duk_require_stack
    , duk_require_stack_top
    , duk_require_string
    , duk_require_top_index
    , duk_require_type_mask
    , duk_require_uint
    , duk_require_undefined
    , duk_require_valid_index
    , duk_safe_to_lstring
    , duk_safe_to_string
    , duk_set_global_object
    , duk_set_top
    , duk_steal_buffer
    , duk_swap
    , duk_swap_top
    , duk_to_boolean
    , duk_to_dynamic_buffer
    , duk_to_fixed_buffer
    , duk_to_int
    , duk_to_int32
    , duk_to_lstring
    , duk_to_null
    , duk_to_number
    , duk_to_object
    , duk_to_pointer
    , duk_to_primitive
    , duk_to_string
    , duk_to_uint
    , duk_to_uint16
    , duk_to_uint32
    , duk_to_undefined
    , duk_xcopy_top
    , duk_xmove_top
      -- * Strings
    , duk_char_code_at
    , duk_concat
    , DukDecodeCharFunction
    , duk_decode_string
    , duk_join
    , duk_map_string
    , duk_substring
    , duk_trim
    ) where

import Data.Bits

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

----------------------------------------------------------------------------------------------------

-- | No type (missing value, invalid index, etc)
foreign import capi "duktape.h value DUK_TYPE_NONE"      c_DUK_TYPE_NONE      :: CInt
-- | ECMAScript @undefined@
foreign import capi "duktape.h value DUK_TYPE_UNDEFINED" c_DUK_TYPE_UNDEFINED :: CInt
-- | ECMAScript @null@
foreign import capi "duktape.h value DUK_TYPE_NULL"      c_DUK_TYPE_NULL      :: CInt
-- | @true@ and @false@
foreign import capi "duktape.h value DUK_TYPE_BOOLEAN"   c_DUK_TYPE_BOOLEAN   :: CInt
-- | IEEE double-precision floating-point number
foreign import capi "duktape.h value DUK_TYPE_NUMBER"    c_DUK_TYPE_NUMBER    :: CInt
-- | Immutable string
foreign import capi "duktape.h value DUK_TYPE_STRING"    c_DUK_TYPE_STRING    :: CInt
-- | Object with properties
foreign import capi "duktape.h value DUK_TYPE_OBJECT"    c_DUK_TYPE_OBJECT    :: CInt
-- | Mutable byte buffer (fixed/dynamic)
foreign import capi "duktape.h value DUK_TYPE_BUFFER"    c_DUK_TYPE_BUFFER    :: CInt
-- | Opaque pointer (ala @void *@)
foreign import capi "duktape.h value DUK_TYPE_POINTER"   c_DUK_TYPE_POINTER   :: CInt
-- | Plain Duktape/C pointer (non-object)
foreign import capi "duktape.h value DUK_TYPE_LIGHTFUNC" c_DUK_TYPE_LIGHTFUNC :: CInt

foreign import capi "duktape.h value DUK_TYPE_MASK_NONE"      c_DUK_TYPE_MASK_NONE      :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_UNDEFINED" c_DUK_TYPE_MASK_UNDEFINED :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_NULL"      c_DUK_TYPE_MASK_NULL      :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_BOOLEAN"   c_DUK_TYPE_MASK_BOOLEAN   :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_NUMBER"    c_DUK_TYPE_MASK_NUMBER    :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_STRING"    c_DUK_TYPE_MASK_STRING    :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_OBJECT"    c_DUK_TYPE_MASK_OBJECT    :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_BUFFER"    c_DUK_TYPE_MASK_BUFFER    :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_POINTER"   c_DUK_TYPE_MASK_POINTER   :: CUInt
foreign import capi "duktape.h value DUK_TYPE_MASK_LIGHTFUNC" c_DUK_TYPE_MASK_LIGHTFUNC :: CUInt

----------------------------------------------------------------------------------------------------

-- | Internal error code: No error (e.g. from @duk_get_error_code()@)
foreign import capi "duktape.h value DUK_ERR_NONE"
  c_DUK_ERR_NONE :: CInt
-- | ECMAScript E5 error code: @Error@
foreign import capi "duktape.h value DUK_ERR_ERROR"
  c_DUK_ERR_ERROR :: CInt
-- | ECMAScript E5 error code: @EvalError@
foreign import capi "duktape.h value DUK_ERR_EVAL_ERROR"
  c_DUK_ERR_EVAL_ERROR :: CInt
-- | ECMAScript E5 error code: @RangeError@
foreign import capi "duktape.h value DUK_ERR_RANGE_ERROR"
  c_DUK_ERR_RANGE_ERROR :: CInt
-- | ECMAScript E5 error code: @ReferenceError@
foreign import capi "duktape.h value DUK_ERR_REFERENCE_ERROR"
  c_DUK_ERR_REFERENCE_ERROR :: CInt
-- | ECMAScript E5 error code: @SyntaxError@
foreign import capi "duktape.h value DUK_ERR_SYNTAX_ERROR"
  c_DUK_ERR_SYNTAX_ERROR :: CInt
-- | ECMAScript E5 error code: @TypeError@
foreign import capi "duktape.h value DUK_ERR_TYPE_ERROR"
  c_DUK_ERR_TYPE_ERROR :: CInt
-- | ECMAScript E5 error code: @URIError@
foreign import capi "duktape.h value DUK_ERR_URI_ERROR"
  c_DUK_ERR_URI_ERROR :: CInt

----------------------------------------------------------------------------------------------------

foreign import capi "duktape.h value DUK_RET_ERROR" 
  c_DUK_RET_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_EVAL_ERROR" 
  c_DUK_RET_EVAL_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_RANGE_ERROR" 
  c_DUK_RET_RANGE_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_REFERENCE_ERROR" 
  c_DUK_RET_REFERENCE_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_SYNTAX_ERROR" 
  c_DUK_RET_SYNTAX_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_TYPE_ERROR" 
  c_DUK_RET_TYPE_ERROR :: CInt
foreign import capi "duktape.h value DUK_RET_URI_ERROR" 
  c_DUK_RET_URI_ERROR :: CInt

----------------------------------------------------------------------------------------------------

foreign import capi "duktape.h value DUK_EXEC_SUCCESS" c_DUK_EXEC_SUCCESS :: CInt
foreign import capi "duktape.h value DUK_EXEC_ERROR"   c_DUK_EXEC_ERROR   :: CInt

----------------------------------------------------------------------------------------------------

-- | Compile eval code (instead of program)
foreign import capi "duktape.h value DUK_COMPILE_EVAL"      c_DUK_COMPILE_EVAL     :: CUInt
-- | Compile function code (instead of program)
foreign import capi "duktape.h value DUK_COMPILE_FUNCTION"  c_DUK_COMPILE_FUNCTION :: CUInt
-- | Use strict (outer) context for pgoram, eval or function
foreign import capi "duktape.h value DUK_COMPILE_STRICT"    c_DUK_COMPILE_STRICT   :: CUInt

----------------------------------------------------------------------------------------------------

-- | Set writable (effective if 'c_DUK_DEFPROP_HAVE_WRITABLE' set).
foreign import capi "duktape.h value DUK_DEFPROP_WRITABLE" 
  c_DUK_DEFPROP_WRITABLE :: DukUInt
-- | Set enumerable (effective if 'c_DUK_DEFPROP_HAVE_ENUMERABLE' set).
foreign import capi "duktape.h value DUK_DEFPROP_ENUMERABLE" 
  c_DUK_DEFPROP_ENUMERABLE :: DukUInt
-- | Set configurable (effective if 'c_DUK_DEFPROP_HAVE_CONFIGURABLE' set).
foreign import capi "duktape.h value DUK_DEFPROP_CONFIGURABLE" 
  c_DUK_DEFPROP_CONFIGURABLE :: DukUInt
-- | Set/clear writeable.
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_WRITABLE" 
  c_DUK_DEFPROP_HAVE_WRITABLE :: DukUInt
-- | Set/clear enumerable.
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_ENUMERABLE" 
  c_DUK_DEFPROP_HAVE_ENUMERABLE :: DukUInt
-- | Set/clear configurable.
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_CONFIGURABLE" 
  c_DUK_DEFPROP_HAVE_CONFIGURABLE :: DukUInt
-- | Set value (given on value stack).
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_VALUE" 
  c_DUK_DEFPROP_HAVE_VALUE :: DukUInt
-- | Set getter (given on value stack).
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_GETTER" 
  c_DUK_DEFPROP_HAVE_GETTER :: DukUInt
-- | Set setter (given on value stack).
foreign import capi "duktape.h value DUK_DEFPROP_HAVE_SETTER" 
  c_DUK_DEFPROP_HAVE_SETTER :: DukUInt
-- | Force change if possible, may still fail for e.g. virtual properties.
foreign import capi "duktape.h value DUK_DEFPROP_FORCE" 
  c_DUK_DEFPROP_FORCE :: DukUInt

c_DUK_DEFPROP_SET_WRITABLE :: DukUInt
c_DUK_DEFPROP_SET_WRITABLE =
  c_DUK_DEFPROP_HAVE_WRITABLE .|. c_DUK_DEFPROP_WRITABLE

c_DUK_DEFPROP_CLEAR_WRITABLE :: DukUInt
c_DUK_DEFPROP_CLEAR_WRITABLE =
  c_DUK_DEFPROP_HAVE_WRITABLE

c_DUK_DEFPROP_SET_ENUMERABLE :: DukUInt
c_DUK_DEFPROP_SET_ENUMERABLE =
  c_DUK_DEFPROP_HAVE_ENUMERABLE .|. c_DUK_DEFPROP_ENUMERABLE

c_DUK_DEFPROP_CLEAR_ENUMERABLE :: DukUInt
c_DUK_DEFPROP_CLEAR_ENUMERABLE =
  c_DUK_DEFPROP_HAVE_ENUMERABLE

c_DUK_DEFPROP_SET_CONFIGURABLE :: DukUInt
c_DUK_DEFPROP_SET_CONFIGURABLE =
  c_DUK_DEFPROP_HAVE_CONFIGURABLE .|. c_DUK_DEFPROP_CONFIGURABLE

c_DUK_DEFPROP_CLEAR_CONFIGURABLE :: DukUInt
c_DUK_DEFPROP_CLEAR_CONFIGURABLE =
  c_DUK_DEFPROP_HAVE_CONFIGURABLE

----------------------------------------------------------------------------------------------------

-- | Enumerate non-enumerable properties in addition to enumerable.
foreign import capi "duktape.h value DUK_ENUM_INCLUDE_NONENUMERABLE" 
  c_DUK_ENUM_INCLUDE_NONENUMERABLE :: DukUInt
-- | Enumrate internal properties (regardless of enumerability).
foreign import capi "duktape.h value DUK_ENUM_INCLUDE_HIDDEN" 
  c_DUK_ENUM_INCLUDE_HIDDEN :: DukUInt
-- | Don't walk prototype chain, only check own properties.
foreign import capi "duktape.h value DUK_ENUM_OWN_PROPERTIES_ONLY" 
  c_DUK_ENUM_OWN_PROPERTIES_ONLY :: DukUInt
-- | Only enumerate array indices.
foreign import capi "duktape.h value DUK_ENUM_ARRAY_INDICES_ONLY" 
  c_DUK_ENUM_ARRAY_INDICES_ONLY :: DukUInt
-- | Sort array indices, use with 'c_DUK_ENUM_ARRAY_INDICES_ONLY'.
foreign import capi "duktape.h value DUK_ENUM_SORT_ARRAY_INDICES" 
  c_DUK_ENUM_SORT_ARRAY_INDICES :: DukUInt
-- | Enumerate a proxy object itself without invoking proxy behavior.
foreign import capi "duktape.h value DUK_ENUM_NO_PROXY_BEHAVIOR" 
  c_DUK_ENUM_NO_PROXY_BEHAVIOR :: DukUInt

----------------------------------------------------------------------------------------------------

-- | Prefer number, unless coercion input is a @Date@, in which case prefer string (E5 Section 8.12.8)
foreign import capi "duktape.h value DUK_HINT_NONE" c_DUK_HINT_NONE :: CInt
-- | Prefer a string
foreign import capi "duktape.h value DUK_HINT_STRING" c_DUK_HINT_STRING :: CInt
-- | Perfer a number
foreign import capi "duktape.h value DUK_HINT_NUMBER" c_DUK_HINT_NUMBER :: CInt

----------------------------------------------------------------------------------------------------

-- | Create a new global environment
foreign import capi "duktape.h value DUK_THREAD_NEW_GLOBAL_ENV"
  c_DUK_THREAD_NEW_GLOBAL_ENV :: CInt

----------------------------------------------------------------------------------------------------

foreign import capi "duktape.h value DUK_INVALID_INDEX"   c_DUK_INVALID_INDEX   :: CInt
foreign import capi "duktape.h value DUK_VARARGS"         c_DUK_VARARGS         :: CInt
foreign import capi "duktape.h value DUK_API_ENTRY_STACK" c_DUK_API_ENTRY_STACK :: CInt

----------------------------------------------------------------------------------------------------

data DukContext

type DukBool      = CInt
type DukInt       = CInt
type DukInt32     = CInt
type DukUInt      = CUInt
type DukUInt16    = CUShort
type DukUInt32    = CUInt
type DukIdx       = CInt
type DukArrIdx    = CInt
type DukUArrIdx   = CUInt
type DukErrCode   = CInt
type DukSize      = CSize
type DukRet       = CInt
type DukDouble    = CDouble
type DukCodePoint = CInt

-- | Like 'duk_alloc_raw', but may trigger a garbage collection to satisfy the request. However, the
-- allocated memory itself  is not automatically garbage collected. The  allocation request may fail
-- even after garbage collection,  in which case 'nullPtr' is returned. The  allocated memory is not
-- automatically zeroed, and may contain arbitrary garbage.
--
-- Memory allocated with 'duk_alloc' can be freed with either 'duk_free' or 'duk_free_raw'.
--
foreign import capi safe "duktape.h duk_alloc"
  duk_alloc :: Ptr DukContext -> DukSize -> IO (Ptr a)

-- | Allocate  a number of  bytes using tht  raw allocation function  registered to the  context. If
-- allocation failes, returns 'nullPtr'. If the requested size is zero, the call may either return a
-- 'nullPtr' or some non-null value which may be safely given to e.g. 'duk_free_raw'. The allocation
-- cannot trigger  a garbage  collection, and  the allocated  memory is  not garbage  collected. The
-- allocated memory is not automatically zeroed, and may contain arbitrary garbage.
--
-- Memory allocated with 'dul_alloc_raw' can be freed with either 'duk_free' or 'duk_free_raw'.
--
foreign import capi safe "duktape.h duk_alloc_raw"
  duk_alloc_raw :: Ptr DukContext -> DukSize -> IO (Ptr a)

-- |  Decodes a  base-64 encoded  value into  a buffer  as an  in-place operation.  If the  input is
-- invalid, throws an error.
--
-- @
-- Stack: [ ..., base64_val, ... ] -> [ ..., val, ... ]
-- @
--
foreign import capi safe "duktape.h duk_base64_decode"
  duk_base64_decode :: Ptr DukContext -> DukIdx -> IO ()

-- |  Coerces an  arbitrary value  into a  buffer and  then encodes  the result  into base-64  as an
-- in-place operation. Returns a pointer to the resulting string for convenience.
--
-- Note: Coercing  a buffer first  coerces a non-buffer  value into a  string, and then  coerces the
-- string into a buffer. The resulting buffer contains the string in CESU-8 encoding.
--
-- @
-- Stack: [ ..., val, ... ] -> [ ..., base64_val, ... ]
-- @
--
foreign import capi safe "duktape.h duk_base64_encode"
  duk_base64_encode :: Ptr DukContext -> DukIdx -> IO CString

-- | Call  target function  @func@ with @nargs@  arguments (not counting  the function  itself). The
-- function and  its arguments are  replaced by a  single return value.  An error thrown  during the
-- function call is not automatically caught.
--
-- The target function @this@ binding is initially set to @undefined@. If the target function is not
-- strict, the binding is replaced by the global  object before the function is invoked. If you want
-- to control the @this@ binding, you can use 'duk_call_method' or 'duk_call_prop' instead.
--
-- @
-- Stack: [ ..., func, arg1, ..., argN ] -> [ ..., retval ]
-- @
--
-- This API call is equivalent to:
--
-- @
-- var retval = func (arg1, ..., argN);
-- @
--
-- or:
--
-- @
-- var retval = func.call (undefined, arg1, ..., argN);
-- @
--
-- Example:
--
-- @
-- ... = do
--   duk_dup ctx func_idx
--   duk_push_int ctx 2
--   duk_push_int ctx 3
--   duk_call ctx 2                      {- [ ... func 2 3 ] -> [ 5 ] -}
--   i <- duk_get_int ctx (negate 1)
--   printf "2 + 3 = %i\n" i
--   duk_pop ctx
-- @
--
foreign import capi safe "duktape.h duk_call"
  duk_call :: Ptr DukContext -> DukIdx -> IO ()

-- |  Call target  function @func@  with  an explicity  @this@  binding and  @nargs@ arguments  (not
-- counting the  function and  the @this@ binding  valid). The function  object, the  @this@ binding
-- value and the  function arguments are replaced by  a single return value. An  error thrown during
-- the function call is not automatically caught.
--
-- If the  target function  is not  strict, the  binding value seen  by the  target function  may be
-- modified by processing specified in ECMAScript E5 10.4.3.
--
-- This API call is equivalent to:
--
-- @
-- var retval = func.call (this_binding, arg1, ..., argN);
-- @
--
-- Example:
--
-- @
-- ... = do
--   duk_push_string ctx func_str
--   duk_eval ctx
--   duk_push_int ctx 123
--   duk_push_int ctx 2
--   duk_push_int ctx 3
--   duk_call_method ctx 2
--   i <- dig_get_int ctx (negate 1)
--   printf "123 + 2 + 3 = %i\n" i
--   duk_pop ctx
--   where
--     func_str = "function (x, y) { return this + x + y; }"
-- @
--
foreign import capi safe "duktape.h duk_call_method"
  duk_call_method :: Ptr DukContext -> DukIdx -> IO ()

-- | Call @obj.key@ with @nargs@ arguments, with @this@  binding set to @obj@. The property name and
-- the function arguments are replaced by a single returnvalue; the target object is not touched. An
-- error during the function call is not automatically caught.
--
-- If the  target function  is not  strict, the  binding value seen  by the  target function  may be
-- modified by processing specified in ECMAScript E5 10.4.3.
--
-- This API call is equivalent to:
--
-- @
-- var retval = obj[key](arg1, ..., argN);
-- @
--
-- or:
--
-- @
-- var func = obj[key];
-- var retval = func.call (obj, arg1, ..., argN);
-- @
--
-- Note: although the base  value for property accesses is usually an object,  it can technically be
-- an arbitrary  value. Plain  string and  buffer values have  virtual index  properties so  you can
-- access @"foo"[2]@, for instance. Most primitive values also inherit from some prototype object so
-- that you can e.g. call methods on them: @(12345).toString(16)@.
--
-- Example:
--
-- @
-- ... = do
--   duk_push_string ctx "myAdderMethod"
--   duk_push_int ctx 2
--   duk_push_int ctx 3
--   duk_call_prop ctx obj_idx 2
--   i <- duk_get_int ctx (negate 1)
--   printf "2 + 3 = %i\n" i
--   duk_pop ctx
-- @
--
foreign import capi safe "duktape.h duk_call_prop"
  duk_call_prop :: Ptr DukContext -> DukIdx -> DukIdx -> IO ()

-- | Given @duk_char_code_at ctx index char_offset@, get the codepoint of a character at a character
-- offset @char_offset@ of a string  at the given @index@. If the value at  @index@ is not a string,
-- an error is thrown. If @char_offset@ is invalid (outside the string) a zero is returned.
--
foreign import capi safe "duktape.h duk_char_code_at"
  duk_char_code_at :: Ptr DukContext -> DukIdx -> DukSize -> IO DukCodePoint

-- | Given  the call @duk_check_stack  ctx extra@, ensure  that the value  stack has at  least extra
-- reserved (allocated) elements for  caller's use, relative to the current stack  top. Returns 1 if
-- successful, 0 otherwise. If the call is  successful, the caller is guaranteed that extra elements
-- can  be pushed  to  the value  stack  without a  value  stack related  error  (other errors  like
-- out-of-memory can still  occur). The caller MUST NOT  rely on being able to push  more than extra
-- values; although this is possible, such elements are reserved for Duktape's internal use.
--
-- Upon entry to  a Duktape/C function and when  outside any call there is an  automatic reserve (of
-- 'c_DUK_API_ENTRY_STACK' elements) allocated  for the caller in addition to  call arguments on the
-- value stack. If more  value stack space is needed, the caller must  reserve more space explicitly
-- either in the beginning of the function (e.g. if  the number of elements required is known or can
-- be computed based on arguments) or dynamically (e.g. inside a loop). Note that an attempt to push
-- a value beyond the  currently allocated value stack causes an error: it  does not cause the value
-- stack to be automatically extended. This simplifies the internal implementation.
--
-- In addition to user reserved elements, Duktape keeps an automatic internal value stack reserve to
-- ensure all API calls have enough value stack space to work without further allocations. The value
-- stack is  also extended in somewhat  large steps to  minimize memory reallocation activity.  As a
-- result the internal  number of value stack  elements available beyond the  caller specified extra
-- varies considerably. The caller does not need to  take this into account and should never rely on
-- any additional elements being available.
--
-- Example:
--
-- @
-- ... = do
--   nargs <- duk_get_top ctx
--   {- Reserve space for one temporary object for each input argument. -}
--   res <- duk_check_stack ctx (2 * nargs)
--   when (res == 0) $
--     fail "Failed to reserve enough stack space"
-- @
--
foreign import capi safe "duktape.h duk_check_stack"
  duk_check_stack :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_check_stack_top"
  duk_check_stack_top :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_check_type"
  duk_check_type :: Ptr DukContext -> DukIdx -> DukInt -> IO DukBool

foreign import capi safe "duktape.h duk_check_type_mask"
  duk_check_type_mask :: Ptr DukContext -> DukIdx -> DukUInt -> IO DukBool

foreign import capi safe "duktape.h duk_compact"
  duk_compact :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_compile"
  duk_compile :: Ptr DukContext -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_compile_lstring"
  duk_compile_lstring :: Ptr DukContext -> DukUInt -> CString -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_compile_lstring_filename"
  duk_compile_lstring_filename :: Ptr DukContext -> DukUInt -> CString -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_compile_string"
  duk_compile_string :: Ptr DukContext -> DukUInt -> CString -> IO ()

foreign import capi safe "duktape.h duk_compile_string_filename"
  duk_compile_string_filename :: Ptr DukContext -> DukUInt -> CString -> IO ()

foreign import capi safe "duktape.h duk_concat"
  duk_concat :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_config_buffer"
  duk_config_buffer :: Ptr DukContext -> DukIdx -> Ptr a -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_copy"
  duk_copy :: Ptr DukContext -> DukIdx -> DukIdx -> IO ()

type DukAllocFunction   a = Ptr a -> DukSize -> IO (Ptr ())
type DukReallocFunction a = Ptr a -> Ptr () -> DukSize -> IO (Ptr ())
type DukFreeFunction    a = Ptr a -> Ptr () -> IO ()
type DukFatalFunction     = Ptr DukContext -> DukInt -> CString -> IO ()

foreign import capi safe "duktape.h duk_create_heap"
  duk_create_heap :: FunPtr (DukAllocFunction   a) ->
                     FunPtr (DukReallocFunction a) ->
                     FunPtr (DukFreeFunction    a) -> Ptr a ->
                     FunPtr DukFatalFunction -> IO (Ptr DukContext)

foreign import capi safe "duktape.h duk_create_heap_default"
  duk_create_heap_default :: IO (Ptr DukContext)

type DukDebugReadFunction       a = Ptr a -> CString -> DukSize -> IO DukSize
type DukDebugWriteFunction      a = Ptr a -> CString -> DukSize -> IO DukSize
type DukDebugPeekFunction       a = Ptr a -> IO DukSize
type DukDebugReadFlushFunction  a = Ptr a -> IO ()
type DukDebugWriteFlushFunction a = Ptr a -> IO ()
type DukDebugRequestFunction    a = Ptr DukContext -> Ptr a -> DukIdx -> IO ()
type DukDebugDetachFunction     a = Ptr DukContext -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_debugger_attach"
  duk_debugger_attach :: Ptr DukContext ->
                         FunPtr (DukDebugReadFunction a)       ->
                         FunPtr (DukDebugWriteFunction a)      ->
                         FunPtr (DukDebugPeekFunction a)       ->
                         FunPtr (DukDebugReadFlushFunction a)  ->
                         FunPtr (DukDebugWriteFlushFunction a) ->
                         FunPtr (DukDebugRequestFunction a)    ->
                         FunPtr (DukDebugDetachFunction a)     -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_debugger_cooperate"
  duk_debugger_cooperate :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_debugger_detach"
  duk_debugger_detach :: Ptr DukContext -> IO ()

type DukDecodeCharFunction a = Ptr a -> DukCodePoint -> IO ()

foreign import capi safe "duktape.h duk_decode_string"
  duk_decode_string :: Ptr DukContext -> DukIdx -> FunPtr (DukDecodeCharFunction a) -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_def_prop"
  duk_def_prop :: Ptr DukContext -> DukIdx -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_del_prop"
  duk_del_prop :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_del_prop_index"
  duk_del_prop_index :: Ptr DukContext -> DukIdx -> DukArrIdx -> IO DukBool

foreign import capi safe "duktape.h duk_del_prop_string"
  duk_del_prop_string :: Ptr DukContext -> DukIdx -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_destroy_heap"
  duk_destroy_heap :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h &duk_destroy_heap"
  duk_destroy_heap_p :: FunPtr (Ptr DukContext -> IO ())

foreign import capi safe "duktape.h duk_dump_function"
  duk_dump_function :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_dup"
  duk_dup :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_dup_top"
  duk_dup_top :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_enum"
  duk_enum :: Ptr DukContext -> DukIdx -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_equals"
  duk_equals :: Ptr DukContext -> DukIdx -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_eval"
  duk_eval :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_eval_lstring"
  duk_eval_lstring :: Ptr DukContext -> CString -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_eval_lstring_noresult"
  duk_eval_lstring_noresult :: Ptr DukContext -> CString -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_eval_noresult"
  duk_eval_noresult :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_eval_string"
  duk_eval_string :: Ptr DukContext -> CString -> IO ()

foreign import capi safe "duktape.h duk_eval_string_noresult"
  duk_eval_string_noresult :: Ptr DukContext -> CString -> IO ()

foreign import capi safe "duktape.h duk_fatal"
  duk_fatal :: Ptr DukContext -> CString -> IO ()

foreign import capi safe "duktape.h duk_free"
  duk_free :: Ptr DukContext -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_free_raw"
  duk_free_raw :: Ptr DukContext -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_gc"
  duk_gc :: Ptr DukContext -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_get_boolean"
  duk_get_boolean :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_get_buffer"
  duk_get_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_get_buffer_data"
  duk_get_buffer_data :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

type DukCFunction = Ptr DukContext -> IO DukRet

foreign import capi safe "duktape.h duk_get_c_function"
  duk_get_c_function :: Ptr DukContext -> DukIdx -> IO (FunPtr DukCFunction)

foreign import capi safe "duktape.h duk_get_context"
  duk_get_context :: Ptr DukContext -> DukIdx -> IO (Ptr DukContext)

foreign import capi safe "duktape.h duk_get_current_magic"
  duk_get_current_magic :: Ptr DukContext -> IO DukInt

foreign import capi safe "duktape.h duk_get_error_code"
  duk_get_error_code :: Ptr DukContext -> DukIdx -> IO DukErrCode

foreign import capi safe "duktape.h duk_get_finalizer"
  duk_get_finalizer :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_get_global_string"
  duk_get_global_string :: Ptr DukContext -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_get_heapptr"
  duk_get_heapptr :: Ptr DukContext -> DukIdx -> IO (Ptr a)

foreign import capi safe "duktape.h duk_get_int"
  duk_get_int :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_get_length"
  duk_get_length :: Ptr DukContext -> DukIdx -> IO DukSize

foreign import capi safe "duktape.h duk_get_lstring"
  duk_get_lstring :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO CString

foreign import capi safe "duktape.h duk_get_magic"
  duk_get_magic :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_get_number"
  duk_get_number :: Ptr DukContext -> DukIdx -> IO DukDouble

foreign import capi safe "duktape.h duk_get_pointer"
  duk_get_pointer :: Ptr DukContext -> DukIdx -> IO (Ptr a)

foreign import capi safe "duktape.h duk_get_prop"
  duk_get_prop :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_get_prop_index"
  duk_get_prop_index :: Ptr DukContext -> DukIdx -> DukArrIdx -> IO DukBool

foreign import capi safe "duktape.h duk_get_prop_string"
  duk_get_prop_string :: Ptr DukContext -> DukIdx -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_get_prototype"
  duk_get_prototype :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_get_string"
  duk_get_string :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_get_top"
  duk_get_top :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_get_top_index"
  duk_get_top_index :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_get_type"
  duk_get_type :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_get_type_mask"
  duk_get_type_mask :: Ptr DukContext -> DukIdx -> IO DukUInt

foreign import capi safe "duktape.h duk_get_uint"
  duk_get_uint :: Ptr DukContext -> DukIdx -> IO DukUInt

foreign import capi safe "duktape.h duk_has_prop"
  duk_has_prop :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_has_prop_index"
  duk_has_prop_index :: Ptr DukContext -> DukIdx -> DukArrIdx -> IO DukBool

foreign import capi safe "duktape.h duk_has_prop_string"
  duk_has_prop_string :: Ptr DukContext -> DukIdx -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_hex_decode"
  duk_hex_decode :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_hex_encode"
  duk_hex_encode :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_insert"
  duk_insert :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_instanceof"
  duk_instanceof :: Ptr DukContext -> DukIdx -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_array"
  duk_is_array :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_boolean"
  duk_is_boolean :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_bound_function"
  duk_is_bound_function :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_buffer"
  duk_is_buffer :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_c_function"
  duk_is_c_function :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_callable"
  duk_is_callable :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_constructor_call"
  duk_is_constructor_call :: Ptr DukContext -> IO DukBool

foreign import capi safe "duktape.h duk_is_dynamic_buffer"
  duk_is_dynamic_buffer :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_ecmascript_function"
  duk_is_ecmascript_function :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_error"
  duk_is_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_eval_error"
  duk_is_eval_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_fixed_buffer"
  duk_is_fixed_buffer :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_function"
  duk_is_function :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_lightfunc"
  duk_is_lightfunc :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_nan"
  duk_is_nan :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_null"
  duk_is_null :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_null_or_undefined"
  duk_is_null_or_undefined :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_number"
  duk_is_number :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_object"
  duk_is_object :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_object_coercible"
  duk_is_object_coercible :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_pointer"
  duk_is_pointer :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_primitive"
  duk_is_primitive :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_range_error"
  duk_is_range_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_reference_error"
  duk_is_reference_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_strict_call"
  duk_is_strict_call :: Ptr DukContext -> IO DukBool

foreign import capi safe "duktape.h duk_is_string"
  duk_is_string :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_syntax_error"
  duk_is_syntax_error :: Ptr DukContext -> DukIdx -> DukBool

foreign import capi safe "duktape.h duk_is_thread"
  duk_is_thread :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_type_error"
  duk_is_type_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_undefined"
  duk_is_undefined :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_uri_error"
  duk_is_uri_error :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_is_valid_index"
  duk_is_valid_index :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_join"
  duk_join :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_json_decode"
  duk_json_decode :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_json_encode"
  duk_json_encode :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_load_function"
  duk_load_function :: Ptr DukContext -> IO ()

type DukMapCharFunction a = Ptr a -> DukCodePoint -> IO DukCodePoint

foreign import capi safe "duktape.h duk_map_string"
  duk_map_string :: Ptr DukContext -> DukIdx -> FunPtr (DukMapCharFunction a) -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_new"
  duk_new :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_next"
  duk_next :: Ptr DukContext -> DukIdx -> DukBool -> IO DukBool

foreign import capi safe "duktape.h duk_normalize_index"
  duk_normalize_index :: Ptr DukContext -> DukIdx -> IO DukIdx

foreign import capi safe "duktape.h duk_pcall"
  duk_pcall :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_pcall_method"
  duk_pcall_method :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_pcall_prop"
  duk_pcall_prop :: Ptr DukContext -> DukIdx -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_pcompile"
  duk_pcompile :: Ptr DukContext -> DukUInt -> IO DukInt

foreign import capi safe "duktape.h duk_pcompile_lstring"
  duk_pcompile_lstring :: Ptr DukContext -> DukUInt -> CString -> DukSize -> IO DukInt

foreign import capi safe "duktape.h duk_pcompile_lstring_filename"
  duk_pcompile_lstring_filename :: Ptr DukContext -> DukUInt -> CString -> DukSize -> IO DukInt

foreign import capi safe "duktape.h duk_pcompile_string"
  duk_pcompile_string :: Ptr DukContext -> DukUInt -> CString -> IO DukInt

foreign import capi safe "duktape.h duk_pcompile_string_filename"
  duk_pcompile_string_filename :: Ptr DukContext -> DukUInt -> CString -> IO DukInt

foreign import capi safe "duktape.h duk_peval"
  duk_peval :: Ptr DukContext -> IO DukInt

foreign import capi safe "duktape.h duk_peval_lstring"
  duk_peval_lstring :: Ptr DukContext -> CString -> DukSize -> IO DukInt

foreign import capi safe "duktape.h duk_peval_lstring_noresult"
  duk_peval_lstring_noresult :: Ptr DukContext -> CString -> DukSize -> IO DukInt

foreign import capi safe "duktape.h duk_peval_noresult"
  duk_peval_noresult :: Ptr DukContext -> IO DukInt

foreign import capi safe "duktape.h duk_peval_string"
  duk_peval_string :: Ptr DukContext -> CString -> IO DukInt

foreign import capi safe "duktape.h duk_peval_string_noresult"
  duk_peval_string_noresult :: Ptr DukContext -> CString -> IO DukInt

foreign import capi safe "duktape.h duk_pnew"
  duk_pnew :: Ptr DukContext -> DukIdx -> IO DukRet

foreign import capi safe "duktape.h duk_pop"
  duk_pop :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_pop_2"
  duk_pop_2 :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_pop_3"
  duk_pop_3 :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_pop_n"
  duk_pop_n :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_push_array"
  duk_push_array :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_push_boolean"
  duk_push_boolean :: Ptr DukContext -> DukBool -> IO ()

foreign import capi safe "duktape.h duk_push_buffer"
  duk_push_buffer :: Ptr DukContext -> DukSize -> DukBool -> IO (Ptr a)

foreign import capi safe "duktape.h duk_push_buffer_object"
  duk_push_buffer_object :: Ptr DukContext -> DukIdx -> DukSize -> DukSize -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_push_c_function"
  duk_push_c_function :: Ptr DukContext -> FunPtr DukCFunction -> DukIdx -> IO DukIdx

foreign import capi safe "duktape.h duk_push_c_lightfunc"
  duk_push_c_lightfunc :: Ptr DukContext -> FunPtr DukCFunction -> DukIdx -> DukIdx -> DukInt -> IO DukIdx

foreign import capi safe "duktape.h duk_push_context_dump"
  duk_push_context_dump :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_current_function"
  duk_push_current_function :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_current_thread"
  duk_push_current_thread :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_dynamic_buffer"
  duk_push_dynamic_buffer :: Ptr DukContext -> DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_push_error_object"
  duk_push_error_object :: Ptr DukContext -> DukErrCode -> CString -> IO DukIdx

foreign import capi safe "duktape.h duk_push_external_buffer"
  duk_push_external_buffer :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_false"
  duk_push_false :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_fixed_buffer"
  duk_push_fixed_buffer :: Ptr DukContext -> DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_push_global_object"
  duk_push_global_object :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_global_stash"
  duk_push_global_stash :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_heap_stash"
  duk_push_heap_stash :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_heapptr"
  duk_push_heapptr :: Ptr DukContext -> Ptr a -> IO DukIdx

foreign import capi safe "duktape.h duk_push_int"
  duk_push_int :: Ptr DukContext -> DukInt -> IO ()

foreign import capi safe "duktape.h duk_push_lstring"
  duk_push_lstring :: Ptr DukContext -> CString -> DukSize -> IO CString

foreign import capi safe "duktape.h duk_push_nan"
  duk_push_nan :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_null"
  duk_push_null :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_number"
  duk_push_number :: Ptr DukContext -> DukDouble -> IO ()

foreign import capi safe "duktape.h duk_push_object"
  duk_push_object :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_push_pointer"
  duk_push_pointer :: Ptr DukContext -> Ptr a -> IO ()

foreign import capi safe "duktape.h duk_push_string"
  duk_push_string :: Ptr DukContext -> CString -> IO CString

foreign import capi safe "duktape.h duk_push_this"
  duk_push_this :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_thread"
  duk_push_thread :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_push_thread_new_globalenv"
  duk_push_thread_new_globalenv :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_push_thread_stash"
  duk_push_thread_stash :: Ptr DukContext -> Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_true"
  duk_push_true :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_push_uint"
  duk_push_uint :: Ptr DukContext -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_push_undefined"
  duk_push_undefined :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_put_global_string"
  duk_put_global_string :: Ptr DukContext -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_put_prop"
  duk_put_prop :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_put_prop_index"
  duk_put_prop_index :: Ptr DukContext -> DukIdx -> DukUArrIdx -> IO DukBool

foreign import capi safe "duktape.h duk_put_prop_string"
  duk_put_prop_string :: Ptr DukContext -> DukIdx -> CString -> IO DukBool

foreign import capi safe "duktape.h duk_realloc"
  duk_realloc :: Ptr DukContext -> Ptr a -> DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_realloc_raw"
  duk_realloc_raw :: Ptr DukContext -> Ptr a -> DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_remove"
  duk_remove :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_replace"
  duk_replace :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_boolean"
  duk_require_boolean :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_require_buffer"
  duk_require_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_require_buffer_data"
  duk_require_buffer_data :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_require_c_function"
  duk_require_c_function :: Ptr DukContext -> DukIdx -> IO (FunPtr DukCFunction)

foreign import capi safe "duktape.h duk_require_callable"
  duk_require_callable :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_context"
  duk_require_context :: Ptr DukContext -> DukIdx -> IO (Ptr DukContext)

foreign import capi safe "duktape.h duk_require_function"
  duk_require_function :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_heapptr"
  duk_require_heapptr :: Ptr DukContext -> DukIdx -> IO (Ptr a)

foreign import capi safe "duktape.h duk_require_int"
  duk_require_int :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_require_lstring"
  duk_require_lstring :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO CString

foreign import capi safe "duktape.h duk_require_normalize_index"
  duk_require_normalize_index :: Ptr DukContext -> DukIdx -> IO DukIdx

foreign import capi safe "duktape.h duk_require_null"
  duk_require_null :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_number"
  duk_require_number :: Ptr DukContext -> DukIdx -> IO DukDouble

foreign import capi safe "duktape.h duk_require_object_coercible"
  duk_require_object_coercible :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_pointer"
  duk_require_pointer :: Ptr DukContext -> DukIdx -> IO (Ptr a)

foreign import capi safe "duktape.h duk_require_stack"
  duk_require_stack :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_stack_top"
  duk_require_stack_top :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_string"
  duk_require_string :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_require_top_index"
  duk_require_top_index :: Ptr DukContext -> IO DukIdx

foreign import capi safe "duktape.h duk_require_type_mask"
  duk_require_type_mask :: Ptr DukContext -> DukIdx -> DukUInt -> IO ()

foreign import capi safe "duktape.h duk_require_uint"
  duk_require_uint :: Ptr DukContext -> DukIdx -> IO DukUInt

foreign import capi safe "duktape.h duk_require_undefined"
  duk_require_undefined :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_require_valid_index"
  duk_require_valid_index :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_resize_buffer"
  duk_resize_buffer :: Ptr DukContext -> DukIdx -> DukSize -> IO (Ptr a)

type DukSafeCallFunction a = Ptr DukContext -> Ptr a -> IO DukRet

foreign import capi safe "duktape.h duk_safe_call"
  duk_safe_call :: Ptr DukContext -> FunPtr (DukSafeCallFunction a) -> Ptr a -> DukIdx -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_safe_to_lstring"
  duk_safe_to_lstring :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO CString

foreign import capi safe "duktape.h duk_safe_to_string"
  duk_safe_to_string :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_set_finalizer"
  duk_set_finalizer :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_set_global_object"
  duk_set_global_object :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_set_magic"
  duk_set_magic :: Ptr DukContext -> DukIdx -> DukInt -> IO ()

foreign import capi safe "duktape.h duk_set_prototype"
  duk_set_prototype :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_set_top"
  duk_set_top :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_steal_buffer"
  duk_steal_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_strict_equals"
  duk_strict_equals :: Ptr DukContext -> DukIdx -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_substring"
  duk_substring :: Ptr DukContext -> DukIdx -> DukSize -> DukSize -> IO ()

foreign import capi safe "duktape.h duk_swap"
  duk_swap :: Ptr DukContext -> DukIdx -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_swap_top"
  duk_swap_top :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_throw"
  duk_throw :: Ptr DukContext -> IO ()

foreign import capi safe "duktape.h duk_to_boolean"
  duk_to_boolean :: Ptr DukContext -> DukIdx -> IO DukBool

foreign import capi safe "duktape.h duk_to_buffer"
  duk_to_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_to_dynamic_buffer"
  duk_to_dynamic_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_to_fixed_buffer"
  duk_to_fixed_buffer :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO (Ptr a)

foreign import capi safe "duktape.h duk_to_int"
  duk_to_int :: Ptr DukContext -> DukIdx -> IO DukInt

foreign import capi safe "duktape.h duk_to_int32"
  duk_to_int32 :: Ptr DukContext -> DukIdx -> IO DukInt32

foreign import capi safe "duktape.h duk_to_lstring"
  duk_to_lstring :: Ptr DukContext -> DukIdx -> Ptr DukSize -> IO CString

foreign import capi safe "duktape.h duk_to_null"
  duk_to_null :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_to_number"
  duk_to_number :: Ptr DukContext -> DukIdx -> IO DukDouble

foreign import capi safe "duktape.h duk_to_object"
  duk_to_object :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_to_pointer"
  duk_to_pointer :: Ptr DukContext -> DukIdx -> IO (Ptr a)

foreign import capi safe "duktape.h duk_to_primitive"
  duk_to_primitive :: Ptr DukContext -> DukIdx -> DukInt -> IO ()

foreign import capi safe "duktape.h duk_to_string"
  duk_to_string :: Ptr DukContext -> DukIdx -> IO CString

foreign import capi safe "duktape.h duk_to_uint"
  duk_to_uint :: Ptr DukContext -> DukIdx -> IO DukUInt

foreign import capi safe "duktape.h duk_to_uint16"
  duk_to_uint16 :: Ptr DukContext -> DukIdx -> IO DukUInt16

foreign import capi safe "duktape.h duk_to_uint32"
  duk_to_uint32 :: Ptr DukContext -> DukIdx -> IO DukUInt32

foreign import capi safe "duktape.h duk_to_undefined"
  duk_to_undefined :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_trim"
  duk_trim :: Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_xcopy_top"
  duk_xcopy_top :: Ptr DukContext -> Ptr DukContext -> DukIdx -> IO ()

foreign import capi safe "duktape.h duk_xmove_top"
  duk_xmove_top :: Ptr DukContext -> Ptr DukContext -> DukIdx -> IO ()


