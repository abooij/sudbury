{-|
Module      : Graphics.Sudbury.Protocol.XML
Description : Parses wayland XML protocol files
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.Protocol.XML where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Text.XML.Light as XML
import System.Process

import Graphics.Sudbury.Protocol.XML.Types
import Graphics.Sudbury.Argument

qname :: String -> XML.QName
qname name = XML.QName name Nothing Nothing

-- | Parse a wayland XML document
parseProtocol :: XML.Element -> Maybe Protocol
parseProtocol doc = do
  docName <- XML.findAttr (qname "name") doc
  description <- parseDescription doc
  let copyright = XML.strContent <$> XML.findChild (qname "copyright") doc
  interfaces <- mapM parseInterface (XML.findChildren (qname "interface") doc)
  return Protocol
    { protocolName        = docName
    , protocolDescription = description
    , protocolInterfaces  = interfaces
    , protocolCopyright   = copyright
    }

-- | Given some XML element, find its `<description>` if it has one.
--   The double Maybe reflects the fact that an element might not have a description,
--   and if it does, it might not be valid (since it needs to have a summary).
parseDescription :: XML.Element -> Maybe (Maybe Description)
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
parseInterface :: XML.Element -> Maybe Interface
parseInterface elt = do
  name <- XML.findAttr (qname "name") elt
  description <- parseDescription elt
  let version  = fromMaybe 0 (XML.findAttr (qname "version") elt >>= readMaybe)
  enums    <- mapM parseEnum (XML.findChildren (qname "enum") elt)
  requests <- mapM (parseMessage version) (XML.findChildren (qname "request") elt)
  events   <- mapM (parseMessage version) (XML.findChildren (qname "event") elt)
  return Interface
    { interfaceName        = name
    , interfaceDescription = description
    , interfaceVersion     = version
    , interfaceRequests    = requests
    , interfaceEvents      = events
    , interfaceEnums       = enums
    }

-- | Parse a `<message>`
parseMessage :: Int -> XML.Element -> Maybe Message
parseMessage parentVersion elt = do
  name <- XML.findAttr (qname "name") elt
  arguments <- mapM parseArgument (XML.findChildren (qname "arg") elt)
  let since = fromMaybe parentVersion (XML.findAttr (qname "since") elt >>= readMaybe)
  destructor <- case XML.findAttr (qname "type") elt of
    Just "destructor" -> Just True
    Nothing           -> Just False
    _                 -> Nothing
  description <- parseDescription elt
  return Message
    { messageName         = name
    , messageArguments    = arguments
    , messageIsDestructor = destructor
    , messageSince        = since
    , messageDescription  = description
    }

-- | Parse an `<arg>`.
parseArgument :: XML.Element -> Maybe Argument
parseArgument elt = do
  name <- XML.findAttr (qname "name") elt
  typestr <- XML.findAttr (qname "type") elt
  let enum = XML.findAttr (qname "enum") elt
  let interface = XML.findAttr (qname "interface") elt
  argtype <- case typestr of
    "int"    -> return $ ArgProtDataBox SIntWAT enum
    "uint"   -> return $ ArgProtDataBox SUIntWAT enum
    "fixed"  -> return $ ArgProtDataBox SFixedWAT ()
    "string" -> return $ ArgProtDataBox SStringWAT ()
    "object" -> return $ ArgProtDataBox SObjectWAT interface
    "new_id" -> return $ ArgProtDataBox SNewIdWAT interface
    "array"  -> return $ ArgProtDataBox SArrayWAT ()
    "fd"     -> return $ ArgProtDataBox SFdWAT ()
    _        -> Nothing
  nullable <- case XML.findAttr (qname "allow-null") elt of
    Nothing      -> Just False
    Just "true"  -> Just True
    Just "false" -> Just False
    Just _       -> Nothing
  let summary = XML.findAttr (qname "summary") elt
  return Argument
    { argumentName     = name
    , argumentType     = argtype
    , argumentNullable = nullable
    , argumentSummary  = summary
    }

-- | Parse an `<enum>`
parseEnum :: XML.Element -> Maybe ArgEnum
parseEnum elt = do
  name <- XML.findAttr (qname "name") elt
  entries <- mapM parseEntry (XML.findChildren (qname "entry") elt)
  bitfield <- case XML.findAttr (qname "bitfield") elt of
                Just "true"  -> Just (Just True)
                Just "false" -> Just (Just False)
                Nothing      -> Just Nothing
                _            -> Nothing
  description <- parseDescription elt
  return ArgEnum
    { enumName        = name
    , enumEntries     = entries
    , enumBitfield    = bitfield
    , enumDescription = description
    }

-- | Parse an enum `<entry>`
parseEntry :: XML.Element -> Maybe Entry
parseEntry elt = do
  name <- XML.findAttr (qname "name") elt
  valueStr <- XML.findAttr (qname "value") elt
  value <- readMaybe valueStr
  let summary = XML.findAttr (qname "summary") elt
  return Entry
    { entryName    = name
    , entryValue   = value
    , entrySummary = summary
    }

-- | Open a protocol file and parse it into a protocol
parseFile :: FilePath -> IO (Maybe Protocol)
parseFile filename = do
  fileContents <- readFile filename
  let doc  = XML.parseXMLDoc fileContents
      prot = doc >>= parseProtocol :: Maybe Protocol
  return prot

-- | locate wayland.xml on disk and parse it
readProtocol :: IO (Maybe Protocol)
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
