{-|
Module      : Graphics.Sudbury.Protocol.Runtime
Description : Protocol data without docs: can project XML protocols to runtime protocols
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.Protocol.Runtime where

import qualified Graphics.Sudbury.Protocol.XML.Types as XML
import qualified Graphics.Sudbury.Protocol.Runtime.Types as RT

projectProtocol :: XML.Protocol -> RT.Protocol
projectProtocol (XML.Protocol _ _ interfaces _) = RT.Protocol $ map projectInterface interfaces

projectInterface :: XML.Interface -> RT.Interface
projectInterface (XML.Interface name _ version requests events enums)
  = RT.Interface
      name
      version
      (map projectMessage requests)
      (map projectMessage events)
      enums

projectMessage :: XML.Message -> RT.Message
projectMessage (XML.Message name args destructor since _)
  = RT.Message
      name
      args
      destructor
      since
