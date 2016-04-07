{-|
Module      : Graphics.Sudbury.CABI.Structs
Description : C ABI
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

We attempt to implement the ABI of the libwayland C library.
This implements proper binding to C structs.
-}
module Graphics.Sudbury.CABI.Structs where

import Data.Word
import Foreign.Ptr
import Foreign.C
import Foreign.Storable

#include <wayland-util.h>

{-
struct wl_message {
        const char *name;
        const char *signature;
        const struct wl_interface **types;
};
-}

data WL_message = WL_message
  { msgName :: CString
  , msgSignature :: CString
  , msgInterfaces :: Ptr (Ptr WL_interface)
  }
instance Storable WL_message where
  sizeOf _ = #{size struct wl_message}
  alignment _ = alignment (0 :: CInt)
  peek p = WL_message
    <$> #{peek struct wl_message, name}      p
    <*> #{peek struct wl_message, signature} p
    <*> #{peek struct wl_message, types}     p
  poke p a = do
    #{poke struct wl_message, name}      p $ msgName  a
    #{poke struct wl_message, signature} p $ msgSignature a
    #{poke struct wl_message, types}     p $ msgInterfaces a
{-
struct wl_interface {
        const char *name;
        int version;
        int method_count;
        const struct wl_message *methods;
        int event_count;
        const struct wl_message *events;
};
-}
data WL_interface = WL_interface
  { ifaceName :: CString
  , ifaceVersion :: CInt
  , ifaceMethodCount :: CInt
  , ifaceMethods :: Ptr WL_message
  , ifaceEventCount :: CInt
  , ifaceEvents :: Ptr WL_message
  }
instance Storable WL_interface where
  sizeOf _ = #{size struct wl_interface}
  alignment _ = alignment (0 :: CInt)
  peek p = WL_interface
    <$> #{peek struct wl_interface, name}  p
    <*> #{peek struct wl_interface, version} p
    <*> #{peek struct wl_interface, method_count} p
    <*> #{peek struct wl_interface, methods} p
    <*> #{peek struct wl_interface, event_count} p
    <*> #{peek struct wl_interface, events} p
  poke p a = do
    #{poke struct wl_interface, name}         p $ ifaceName  a
    #{poke struct wl_interface, version}      p $ ifaceVersion a
    #{poke struct wl_interface, method_count} p $ ifaceMethodCount a
    #{poke struct wl_interface, methods}      p $ ifaceMethods a
    #{poke struct wl_interface, event_count}  p $ ifaceEventCount a
    #{poke struct wl_interface, events}       p $ ifaceEvents a

{-
union wl_argument {
	int32_t i; /**< signed integer */
	uint32_t u; /**< unsigned integer */
	wl_fixed_t f; /**< fixed point */
	const char *s; /**< string */
	struct wl_object *o; /**< object */
	uint32_t n; /**< new_id */
	struct wl_array *a; /**< array */
	int32_t h; /**< file descriptor */
};
-}
-- TODO implement

data WL_arg

wl_arg_size :: Int
wl_arg_size = #{size union wl_argument}

{-
struct wl_array {
	size_t size;
	size_t alloc;
	void *data;
};
void
wl_array_init(struct wl_array *array);

void
wl_array_release(struct wl_array *array);

void *
wl_array_add(struct wl_array *array, size_t size);

int
wl_array_copy(struct wl_array *array, struct wl_array *source);
-}
data WL_array = WL_array
  { arraySize :: #{type size_t}
  , arrayAlloc :: #{type size_t}
  , arrayData :: Ptr CChar
  }
instance Storable WL_array where
  sizeOf _ = #{size struct wl_array}
  alignment _ = alignment (0 :: CInt)
  peek p = WL_array
    <$> #{peek struct wl_array, size}  p
    <*> #{peek struct wl_array, alloc} p
    <*> #{peek struct wl_array, data}  p
  poke p a = do
    #{poke struct wl_array, size}  p $ arraySize  a
    #{poke struct wl_array, alloc} p $ arrayAlloc a
    #{poke struct wl_array, data}  p $ arrayData  a
