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
import Data.Fix

import Graphics.Sudbury.Argument
import Graphics.Sudbury.Protocol.XML.Types
import Graphics.Sudbury.Protocol.Knot.Types

data KnottingException = NoSuchInterfaceException String | NoSuchEnumException String
  deriving (Typeable)
instance Show KnottingException where
  show (NoSuchInterfaceException name) = "No such interface " ++ name
  show (NoSuchEnumException name) = "No such enum " ++ name
instance Exception KnottingException

type DirectInterface = Interface ArgEnum KnotInterface
tieProtocol :: forall m . (MonadFix m, MonadThrow m) => XMLProtocol -> m KnotProtocol
tieProtocol (Protocol n d oldInterfaces c) = (\i -> Protocol n d i c) <$> mfix go
  where
    go :: [DirectInterface] -> m [DirectInterface]
    go newInterfaces = mapM tieInterface oldInterfaces
      where
        -- newInterfaces :: m [Interface]
        -- newInterfaces = mapM tieInterface oldInterfaces
        tieInterface :: XMLInterface -> m DirectInterface
        tieInterface (Interface iname descr version requests events enums)
          = do
            requests' <- mapM tieMessage requests
            events' <- mapM tieMessage events
            return $ Interface
              iname
              descr
              version
              requests'
              events'
              enums
          where
            tieMessage :: XMLMessage -> m KnotMessage
            tieMessage (Message name args destructor since mdescr)
              = do
                args' <- mapM tieArgument args
                return $ Message
                  name
                  args'
                  destructor
                  since
                  mdescr
            tieArgument :: XMLArgument -> m KnotArgument
            tieArgument (Argument name argtype nullable summary)
              = do
                argtype' <- case argtype of
                  ArgProtDataBox tp x -> ArgProtDataBox tp <$> (tieArgType tp x)
                return $ Argument
                  name
                  argtype'
                  nullable
                  summary
            tieArgType :: SArgumentType t -> ArgProtData String String t -> m (ArgProtData ArgEnum KnotInterface t)
            tieArgType SIntWAT x = enumByName x
            tieArgType SUIntWAT x = enumByName x
            tieArgType SFixedWAT x = return x
            tieArgType SStringWAT x = return x
            tieArgType SObjectWAT x = interfaceByName x
            tieArgType SNewIdWAT x = interfaceByName x
            tieArgType SArrayWAT x = return x
            tieArgType SFdWAT x = return x
            interfaceByName :: Maybe String -> m (Maybe KnotInterface)
            interfaceByName (Just name) =
              case findIndex ((== name) . interfaceName) oldInterfaces of
                Nothing -> throwM $ NoSuchInterfaceException name
                Just i  -> return $ Just $ Fix $ newInterfaces !! i
            interfaceByName Nothing = return Nothing
            enumByName :: Maybe String -> m (Maybe ArgEnum)
            enumByName (Just name)
              | '.' `elem` name =
                  case do
                       dotIdx <- '.' `elemIndex` name
                       let ifaceName = take dotIdx name
                           eName = drop (dotIdx + 1) name
                       iface <- find ((== ifaceName) . interfaceName) oldInterfaces
                       find ((== eName) . enumName) (interfaceEnums iface)
                    of
                    Nothing -> throwM $ NoSuchEnumException name
                    Just x  -> return $ Just x
              | otherwise =
                  case find ((== name) . enumName) enums of
                    Nothing -> throwM $ NoSuchEnumException name
                    Just x  -> return $ Just x
            enumByName Nothing = return Nothing

untieProtocol :: KnotProtocol -> XMLProtocol
untieProtocol (Protocol n d oldInterfaces c) = Protocol n d newInterfaces c
  where
    newInterfaces :: [XMLInterface]
    newInterfaces = map untieInterface oldInterfaces
    untieInterface :: DirectInterface -> XMLInterface
    untieInterface (Interface iname descr version requests events enums)
      = Interface
          iname
          descr
          version
          (map untieMessage requests)
          (map untieMessage events)
          enums
      where
        untieMessage :: KnotMessage -> XMLMessage
        untieMessage (Message name args mdescr destructor since)
          = Message
              name
              (map untieArgument args)
              mdescr
              destructor
              since
        untieArgument :: KnotArgument -> XMLArgument
        untieArgument (Argument name argtype nullable summary)
          = Argument
              name
              (case argtype of
                 ArgProtDataBox tp x -> ArgProtDataBox tp (untieArgType tp x))
              nullable
              summary
        untieArgType :: SArgumentType t -> ArgProtData ArgEnum KnotInterface t -> ArgProtData String String t
        untieArgType SIntWAT Nothing = Nothing
        untieArgType SIntWAT (Just x) = Just $ enumName x
        untieArgType SUIntWAT Nothing = Nothing
        untieArgType SUIntWAT (Just x) = Just $ enumName x
        untieArgType SFixedWAT x = x
        untieArgType SStringWAT x = x
        untieArgType SObjectWAT Nothing = Nothing
        untieArgType SObjectWAT (Just x) = Just $ interfaceName $ unFix x
        untieArgType SNewIdWAT Nothing = Nothing
        untieArgType SNewIdWAT (Just x) = Just $ interfaceName $ unFix x
        untieArgType SArrayWAT x = x
        untieArgType SFdWAT x = x
