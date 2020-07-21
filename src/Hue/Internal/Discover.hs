-- |
-- Module: Hue.Internal.Discover
-- Copyright: (c) 2018 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Utility functions for automatically discovering a Bridge IP address.
--
-- This is an internal module.
--
-- Please use "Hue.Discover" instead.
module Hue.Internal.Discover (
    upnpDiscoverBridges
  , nupnpDiscoverBridges
  , XMLParseException(..)
  , parseDescriptionXML
) where

import Prelude hiding (putStr, putStrLn)
import Network.Socket
import Network.Socket.ByteString
import Network.HTTP.Simple
import Network.URI

import qualified Text.XML.Hexml as XML

import System.IO
import Data.Time.Clock

import Data.Typeable
import Data.Maybe
import Data.Monoid()
import Data.Bifunctor
import Data.String

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)

import Data.ByteString (ByteString, append, hGetNonBlocking)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BS

import Control.Concurrent (threadDelay)
import Control.Exception.Safe

import Hue.Internal

-- | Discover bridges on the local network via N-UPnP.
-- Listens for 8 seconds for any responses.
--
-- For any bridge that responds, this function fetches it's @description.xml@ via HTTP
-- and parses the serial number and icon URL.
nupnpDiscoverBridges :: IO [Bridge]
nupnpDiscoverBridges = do
  response <- httpJSON "https://discovery.meethue.com"
  case getResponseStatusCode response of
    200 ->
        let responseContent = getResponseBody response :: [Map.Map Text Text]
            toMaybeBridgeLocation o = do
              bridgeId <- o Map.!? "id"
              bridgeIp <- o Map.!? "internalipaddress"
              bridgeIpDescriptionXmlUri <- parseURI . Text.unpack $ ("http://" <> bridgeIp <> "/description.xml")
              return $ BridgeLocation bridgeId bridgeIpDescriptionXmlUri
            bridgLocations  = mapMaybe toMaybeBridgeLocation responseContent
        in traverse fetchBridge bridgLocations
    _ -> return []


-- | Discover bridges on the local network via UPnP / SSDP.
-- Listens for 5 seconds for any responses.
--
-- For any bridge that responds, this function fetches it's @description.xml@ via HTTP
-- and parses the serial number and icon URL.
upnpDiscoverBridges :: IO [Bridge]
upnpDiscoverBridges = do
  addr:_ <- getAddrInfo (Just addrHints) (Just "239.255.255.250") (Just "1900")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  _ <- sendTo sock ssdpDiscoverRequest (addrAddress addr)
  hSock <- socketToHandle sock ReadMode
  startTime <- getCurrentTime
  bridgLocations <- Set.toList <$> receiveBridgeLocation startTime hSock
  hClose hSock
  traverse fetchBridge bridgLocations

  where
    receiveBridgeLocation :: UTCTime -> Handle -> IO (Set BridgeLocation)
    receiveBridgeLocation startTime hSock = do
      now <- getCurrentTime
      if diffUTCTime now startTime > 5
        then pure Set.empty
        else do
          threadDelay 100000
          bridge <- Set.fromList . maybeToList . processMessage <$> hGetNonBlocking hSock 1024
          moreBridges <- receiveBridgeLocation startTime hSock
          pure $ bridge <> moreBridges

    processMessage :: ByteString -> Maybe BridgeLocation
    processMessage msg =
      BridgeLocation
      <$> Map.lookup "hue-bridgeid" fieldsMap
      <*> location
      where
        packetLines = Text.lines $ decodeUtf8 msg
        rawFields = Text.breakOn ":" <$> packetLines
        packetFields = fmap (Text.strip . Text.drop 1) <$> rawFields
        fieldsMap = Map.fromList packetFields
        location = Map.lookup "LOCATION" fieldsMap >>= parseURI . Text.unpack

    ssdpDiscoverRequest =
               "M-SEARCH * HTTP/1.1\n"
      `append` "HOST: 239.255.255.250\n"
      `append` "MAN: ssdp:discover\n"
      `append` "MX: 5\n"
      `append` "ST: ssdp:all\n"

    addrHints = defaultHints {
        addrFlags = []
      , addrSocketType = Datagram
      , addrFamily = AF_INET
      , addrProtocol = udpProtocolNumber }

    udpProtocolNumber = 17

fetchBridge :: BridgeLocation -> IO Bridge
fetchBridge BridgeLocation{..} = do
  response <- httpLBS httpReq
  let bridge = parseDescriptionXML $ toStrict $ getResponseBody response
  either throw pure bridge

  where
    Just host = uriRegName <$> uriAuthority getBridgeLocation
    httpReq = setRequestHost (fromString host)
            $ setRequestPath (fromString $ uriPath getBridgeLocation)
            $ defaultRequest

-- | Identifies the network location of a bridge.
-- This data is directly constructed from a SSDP response.
data BridgeLocation = BridgeLocation {
  getBridgeID :: Text
, getBridgeLocation :: URI
} deriving (Show, Eq, Ord)

-- | Parsing a bridge @description.xml@ might throw an exception.
data XMLParseException = XMLParseException ByteString deriving (Show, Typeable)

instance Exception XMLParseException

-- | Parse the contents of a bridges' @description.xml@.
parseDescriptionXML :: ByteString -> Either XMLParseException Bridge
parseDescriptionXML str = do
  root <- first XMLParseException (XML.parse str) >>= findChild "root"

  bridgeIP <- findChild "URLBase" root
    >>= onExcept "Invalid bridge URL" . parseURI . BS.unpack . XML.inner
    >>= onExcept "Invalid bridge URL" . uriAuthority
    >>= pure . fromString . uriRegName

  serialNumber <- findChild "device" root
    >>= findChild "serialNumber"
    >>= pure . decodeUtf8 . XML.inner

  iconURL <- findChild "device" root
    >>= findChild "iconList"
    >>= findChild "icon"
    >>= findChild "url"
    >>= pure . decodeUtf8 . XML.inner

  pure $ Bridge (BridgeIP bridgeIP) serialNumber iconURL

  where
    findChild :: ByteString -> XML.Node -> Either XMLParseException XML.Node
    findChild name node = case XML.childrenBy node name of
      [] -> Left $ XMLParseException $
                    "Error parsing Bridge description.xml:\n\
                    \Could not find'" `append` name `append` "' field."
      (x:_) -> Right x

    onExcept :: ByteString -> Maybe a -> Either XMLParseException a
    onExcept _ (Just a) = Right a
    onExcept err Nothing = Left $ XMLParseException $ "Error parsing Bridge description.xml:\n" `append` err
