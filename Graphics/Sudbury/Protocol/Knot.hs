{-|
Module      : Graphics.Sudbury.Protocol.Knot
Description : Tie the knot of wayland protocol data
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Sudbury.Protocol.Knot where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Fix
import Data.Typeable
import Data.List (find, findIndex, elemIndex)

import Graphics.Sudbury.Argument
import qualified Graphics.Sudbury.Protocol.Runtime.Types as RT
import qualified Graphics.Sudbury.Protocol.Knot.Types as KT

data KnottingException = NoSuchInterfaceException String | NoSuchEnumException String
  deriving (Typeable)
instance Show KnottingException where
  show (NoSuchInterfaceException name) = "No such interface " ++ name
  show (NoSuchEnumException name) = "No such enum " ++ name
instance Exception KnottingException


tieProtocol :: forall m . (MonadFix m, MonadThrow m) => RT.Protocol -> m KT.Protocol
tieProtocol (RT.Protocol oldInterfaces) = KT.Protocol <$> mfix go
  where
    go :: [KT.Interface] -> m [KT.Interface]
    go newInterfaces = mapM tieInterface oldInterfaces
      where
        -- newInterfaces :: m [KT.Interface]
        -- newInterfaces = mapM tieInterface oldInterfaces
        tieInterface :: RT.Interface -> m KT.Interface
        tieInterface (RT.Interface iname version requests events enums)
          = do
            requests' <- mapM tieMessage requests
            events' <- mapM tieMessage events
            return $ KT.Interface
              iname
              version
              requests'
              events'
              enums
          where
            tieMessage :: RT.Message -> m KT.Message
            tieMessage (RT.Message name args destructor since)
              = do
                args' <- mapM tieArgument args
                return $ KT.Message
                  name
                  args'
                  destructor
                  since
            tieArgument :: RT.Argument -> m KT.Argument
            tieArgument (RT.Argument name argtype nullable summary)
              = do
                argtype' <- case argtype of
                  RT.ArgProtDataBox tp x -> KT.ArgProtDataBox tp <$> (tieArgType tp x)
                return $ KT.Argument
                  name
                  argtype'
                  nullable
                  summary
            tieArgType :: SArgumentType t -> RT.ArgProtData t -> m (KT.ArgProtData t)
            tieArgType SIntWAT x = enumByName x
            tieArgType SUIntWAT x = enumByName x
            tieArgType SFixedWAT x = return x
            tieArgType SStringWAT x = return x
            tieArgType SObjectWAT x = interfaceByName x
            tieArgType SNewIdWAT x = interfaceByName x
            tieArgType SArrayWAT x = return x
            tieArgType SFdWAT x = return x
            interfaceByName :: Maybe String -> m (Maybe KT.Interface)
            interfaceByName (Just name) =
              case findIndex ((== name) . RT.interfaceName) oldInterfaces of
                Nothing -> throwM $ NoSuchInterfaceException name
                Just i  -> return $ Just $ newInterfaces !! i
            interfaceByName Nothing = return Nothing
            enumByName :: Maybe String -> m (Maybe KT.ArgEnum)
            enumByName (Just name)
              | '.' `elem` name =
                  case do
                       dotIdx <- '.' `elemIndex` name
                       let ifaceName = take dotIdx name
                           enumName = drop (dotIdx + 1) name
                       iface <- find ((== ifaceName) . RT.interfaceName) oldInterfaces
                       find ((== enumName) . RT.enumName) (RT.interfaceEnums iface)
                    of
                    Nothing -> throwM $ NoSuchEnumException name
                    Just x  -> return $ Just x
              | otherwise =
                  case find ((== name) . KT.enumName) enums of
                    Nothing -> throwM $ NoSuchEnumException name
                    Just x  -> return $ Just x
            enumByName Nothing = return Nothing

untieProtocol :: KT.Protocol -> RT.Protocol
untieProtocol (KT.Protocol oldInterfaces) = RT.Protocol newInterfaces
  where
    newInterfaces :: [RT.Interface]
    newInterfaces = map untieInterface oldInterfaces
    untieInterface :: KT.Interface -> RT.Interface
    untieInterface (KT.Interface iname version requests events enums)
      = RT.Interface
          iname
          version
          (map untieMessage requests)
          (map untieMessage events)
          enums
      where
        untieMessage :: KT.Message -> RT.Message
        untieMessage (KT.Message name args destructor since)
          = RT.Message
              name
              (map untieArgument args)
              destructor
              since
        untieArgument :: KT.Argument -> RT.Argument
        untieArgument (KT.Argument name argtype nullable summary)
          = RT.Argument
              name
              (case argtype of
                 KT.ArgProtDataBox tp x -> RT.ArgProtDataBox tp (untieArgType tp x))
              nullable
              summary
        untieArgType :: SArgumentType t -> KT.ArgProtData t -> RT.ArgProtData t
        untieArgType SIntWAT Nothing = Nothing
        untieArgType SIntWAT (Just x) = Just (KT.enumName x)
        untieArgType SUIntWAT Nothing = Nothing
        untieArgType SUIntWAT (Just x) = Just (KT.enumName x)
        untieArgType SFixedWAT x = x
        untieArgType SStringWAT x = x
        untieArgType SObjectWAT Nothing = Nothing
        untieArgType SObjectWAT (Just x) = Just (KT.interfaceName x)
        untieArgType SNewIdWAT Nothing = Nothing
        untieArgType SNewIdWAT (Just x) = Just (KT.interfaceName x)
        untieArgType SArrayWAT x = x
        untieArgType SFdWAT x = x
