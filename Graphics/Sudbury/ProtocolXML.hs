{-|
Module      : Graphics.Sudbury.ProtocolXML
Description : Parses wayland XML protocol files
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.ProtocolXML where

import Debug.Trace
import Control.Monad (filterM)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import qualified Text.XML.Light as XML
import System.Process

type WLDescription = (String , String)

data WLProtocol = WLProtocol
  { protocolName :: String
  , protocolDescription :: Maybe WLDescription
  , protocolInterfaces :: [WLInterface]
  , protocolCopyright :: Maybe String
  } deriving (Eq, Show)

data WLInterface = WLInterface
  { interfaceName :: String
  , interfaceDescription :: Maybe WLDescription
  , interfaceVersion :: Int
  , interfaceSince :: Int
  , interfaceRequests :: [WLMessage]
  , interfaceEvents :: [WLMessage]
  , interfaceEnums :: [WLEnum]
  }
-- The instances for `WLInterface` have to be written manually, to avoid loops
-- in our circular data structure.
-- This is because WLArgumentType refers back to WLInterface
instance Eq WLInterface where
  iface1 == iface2 =
    (interfaceName iface1 == interfaceName iface2) &&
    (interfaceVersion iface1 == interfaceVersion iface2)
instance Show WLInterface where
  show iface =
    "WLInterface " ++
    interfaceName iface ++
    " (version " ++
    show (interfaceVersion iface) ++
    ")"

data WLMessage = WLMessage
  { messageName :: String
  , messageArguments :: [WLArgument]
  , messageIsDestructor :: Bool
  , messageSince :: Int
  , messageDescription :: Maybe WLDescription
  } deriving (Eq, Show)

data WLEnum = WLEnum
  { enumName :: String
  , enumEntries :: [WLEntry]
  , enumBitfield :: Maybe Bool
  , enumDescription :: Maybe WLDescription
  } deriving (Eq, Show)

data WLEntry = WLEntry
  { entryName :: String
  , entryValue :: Integer
  , entrySummary :: Maybe String
  } deriving (Eq, Show)

data WLArgumentType = WLInt (Maybe WLEnum)
                    | WLUInt (Maybe WLEnum)
                    | WLFixed
                    | WLString
                    | WLObject (Maybe WLInterface)
                    | WLNewId (Maybe WLInterface)
                    | WLArray
                    | WLFD
                    deriving (Eq, Show)

data WLArgument = WLArgument
  { argumentName :: String
  , argumentType :: WLArgumentType
  , argumentNullable :: Bool
  , argumentSummary :: Maybe String
  } deriving (Eq, Show)

qname :: String -> XML.QName
qname name = XML.QName name Nothing Nothing

-- The following two functions are also needed to circumvent infinite recursion
-- I don't quite like them, because they generate a new `WLInterface`/`WLEnum` for every call.
-- We should be able to reuse the ones from the tree that we are building up anyway,
-- so as to make an honest circular data structure.

getInterface :: XML.Element -> String -> Maybe WLInterface
getInterface doc name = do
  let elts = XML.findChildren (qname "interface") doc
  ifaceElt <- filterM (\elt -> do
    name' <- XML.findAttr (qname "name") elt
    return (name == name')
    ) elts
  listToMaybe ifaceElt >>= parseInterface doc

getEnum :: XML.Element -> String -> String -> Maybe WLEnum
getEnum doc iname ename = do
  let elts = XML.findChildren (qname "interface") doc
  ifaceElts <- filterM (\elt -> do
    name' <- XML.findAttr (qname "name") elt
    return (iname == name')
    ) elts
  ifaceElt <- listToMaybe ifaceElts
  enumElt <- filterM (\elt -> do
    name' <- XML.findAttr (qname "name") elt
    return (ename == name')
    ) (XML.findChildren (qname "enum") ifaceElt)
  listToMaybe enumElt >>= parseEnum

-- | Parse a wayland XML document
parseProtocol :: XML.Element -> Maybe WLProtocol
parseProtocol doc = do
  docName <- XML.findAttr (qname "name") doc
  description <- parseDescription doc
  let copyright  = XML.strContent <$> XML.findChild (qname "copyright") doc
  interfaces <- mapM (parseInterface doc) (XML.findChildren (qname "interface") doc)
  return WLProtocol
    { protocolName        = docName
    , protocolDescription = description
    , protocolInterfaces  = interfaces
    , protocolCopyright   = copyright
    }

-- | Given some XML element, find its `<description>` if it has one.
--   The double Maybe reflects the fact that an element might not have a description,
--   and if it does, it might not be valid (since it needs to have a summary).
parseDescription :: XML.Element -> Maybe (Maybe WLDescription)
parseDescription elt = do
  let descElts = XML.findChildren (qname "description") elt
  case descElts of
    []        -> return Nothing
    desc : _ -> do
      summary <- XML.findAttr (qname "summary") desc
      return $ Just (summary , XML.strContent desc)

-- | Parse an `<interface>`.
--
--   It is given the list of interfaces in this protocol to cross-reference enum attributes.
--   Note that this circularity is resolved using `mfix` in `parseProtocol`.
parseInterface :: XML.Element -> XML.Element -> Maybe WLInterface
parseInterface doc elt = do
  name <- XML.findAttr (qname "name") elt
  trace name $ return ()
  description <- parseDescription elt
  let version  = fromMaybe 0 (XML.findAttr (qname "version") elt >>= readMaybe)
      since    = fromMaybe 1 (XML.findAttr (qname "since") elt >>= readMaybe)
  enums    <- mapM parseEnum (XML.findChildren (qname "enum") elt)
  requests <- mapM (parseMessage enums version doc) (XML.findChildren (qname "request") elt)
  events   <- mapM (parseMessage enums version doc) (XML.findChildren (qname "event") elt)
  return WLInterface
    { interfaceName        = name
    , interfaceDescription = description
    , interfaceVersion     = version
    , interfaceSince       = since
    , interfaceRequests    = requests
    , interfaceEvents      = events
    , interfaceEnums       = enums
    }

-- | Parse a `<message>`
parseMessage :: [WLEnum] -> Int -> XML.Element -> XML.Element -> Maybe WLMessage
parseMessage enums parentVersion doc elt = do
  name <- XML.findAttr (qname "name") elt
  arguments <- mapM (parseArgument enums doc) (XML.findChildren (qname "arg") elt)
  let since = fromMaybe parentVersion (XML.findAttr (qname "since") elt >>= readMaybe)
  destructor <- case XML.findAttr (qname "type") elt of
    Just "destructor" -> Just True
    Nothing           -> Just False
    _                 -> Nothing
  description <- parseDescription elt
  return WLMessage
    { messageName         = name
    , messageArguments    = arguments
    , messageIsDestructor = destructor
    , messageSince        = since
    , messageDescription  = description
    }

-- | Parse an `<arg>`.
parseArgument :: [WLEnum] -> XML.Element -> XML.Element -> Maybe WLArgument
parseArgument enums doc elt = do
  name <- XML.findAttr (qname "name") elt
  typestr <- XML.findAttr (qname "type") elt
  enum <- case XML.findAttr (qname "enum") elt of
            Nothing  -> Just Nothing
            Just str -> Just <$> findEnum str

  let interface = XML.findAttr (qname "interface") elt >>= getInterface doc
  argtype <- case typestr of
    "int"    -> return (WLInt enum)
    "uint"   -> return (WLUInt enum)
    "fixed"  -> return WLFixed
    "string" -> return WLString
    "object" -> return (WLObject interface)
    "new_id" -> return (WLNewId interface)
    "array"  -> return WLArray
    "fd"     -> return WLFD
    _        -> Nothing
  nullable <- case XML.findAttr (qname "allow-null") elt of
    Nothing      -> Just False
    Just "true"  -> Just True
    Just "false" -> Just False
    Just _       -> Nothing
  let summary = XML.findAttr (qname "summary") elt
  return WLArgument
    { argumentName     = name
    , argumentType     = argtype
    , argumentNullable = nullable
    , argumentSummary  = summary
    }
  where
    splitAttr :: String -> (Maybe String, String)
    splitAttr []         = (Nothing , [])
    splitAttr ('.' : ss) = (Just "" , ss)
    splitAttr (s : ss)   =
      case splitAttr ss of
        (Just i , e) -> (Just $ s : i , e)
        (Nothing , e) -> (Nothing , s : e)

    findEnum :: String -> Maybe WLEnum
    findEnum name =
      case splitAttr name of
        (Nothing , e) -> find (\enum -> enumName enum == e) enums
        (Just i  , e) -> getEnum doc i e

-- | Parse an `<enum>`
parseEnum :: XML.Element -> Maybe WLEnum
parseEnum elt = do
  name <- XML.findAttr (qname "name") elt
  entries <- mapM parseEntry (XML.findChildren (qname "entry") elt)
  bitfield <- case XML.findAttr (qname "bitfield") elt of
                Just "true"  -> Just (Just True)
                Just "false" -> Just (Just False)
                Nothing      -> Just Nothing
                _            -> Nothing
  description <- parseDescription elt
  return WLEnum
    { enumName        = name
    , enumEntries     = entries
    , enumBitfield    = bitfield
    , enumDescription = description
    }

-- | Parse an enum `<entry>`
parseEntry :: XML.Element -> Maybe WLEntry
parseEntry elt = do
  name <- XML.findAttr (qname "name") elt
  valueStr <- XML.findAttr (qname "value") elt
  value <- readMaybe valueStr
  let summary = XML.findAttr (qname "summary") elt
  return WLEntry
    { entryName    = name
    , entryValue   = value
    , entrySummary = summary
    }

-- | Open a protocol file and parse it into a protocol
parseFile :: FilePath -> IO (Maybe WLProtocol)
parseFile filename = do
  fileContents <- readFile filename
  let doc  = XML.parseXMLDoc fileContents
      prot = doc >>= parseProtocol :: Maybe WLProtocol
  return prot

-- | locate wayland.xml on disk and parse it
readProtocol :: IO (Maybe WLProtocol)
readProtocol = do
  filename <- figureOutWaylandFile
  parseFile filename

-- | Use `pkg-config` to locate wayland's data directory
figureOutWaylandDataDir :: IO String
figureOutWaylandDataDir =
  head <$> lines <$> readProcess "pkg-config" ["wayland-server", "--variable=pkgdatadir"] []

-- | Use `pkg-config` to locate `wayland.xml`
figureOutWaylandFile :: IO String
figureOutWaylandFile = do
  dir <- figureOutWaylandDataDir
  return (dir ++ "/" ++ protocolFile)

protocolFile :: FilePath
protocolFile = "wayland.xml"
