{-|
Module      : Graphics.Sudbury.Crap.Structs
Description : C API crap
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

The wayland C API has some free crap included in it.
This implements proper binding to C structs.
-}
module Graphics.Sudbury.Crap.Structs where

import Foreign.Ptr
import Foreign.C
import Foreign.Storable

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
  , msgInterface :: Ptr (Ptr WL_interface)
  }

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
-- TODO implement

data WL_interface = WL_interface
  { ifaceName :: CString
  , ifaceVersion :: CInt
  , ifaceMethodCount :: CInt
  , ifaceMethods :: Ptr WL_message
  , ifaceEventCount :: CInt
  , ifaceEvents :: Ptr WL_message
  }
instance Storable WL_interface where
  sizeOf _ = undefined
  alignment _ = undefined
  peek _ = undefined
  poke _ _ = undefined

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
