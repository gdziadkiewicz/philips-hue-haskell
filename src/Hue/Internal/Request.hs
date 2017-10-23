-- |
-- Module: Hue.Internal.Request 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types to build representations for Hue API requests. 
-- 
-- This is an internal module.
-- 
-- Please use "Hue.Request" instead. 
{-# LANGUAGE GADTs #-}
module Hue.Internal.Request (
  module Hue.Internal.Request
, module MethodTypes
) where

import Data.Aeson hiding (Result(..))

import Data.String (fromString)
import Data.Text (Text, append)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)


import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Blaze.ByteString.Builder (toByteString)

import Network.HTTP.Types.URI (encodePathSegments)
import Network.HTTP.Types.Method as MethodTypes (StdMethod(..))
import Network.HTTP.Simple hiding (JSONParseException, Proxy, Request)
import qualified Network.HTTP.Simple as HTTP

import Hue.Internal

import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader


-- | Make a GET request
get :: Result result -> RequestPath -> Hue result
get = request GET NoBody

-- | Make a POST request
post :: Body body -> Result result -> RequestPath -> Hue result
post = request POST

-- | Make a PUT request
put :: Body body -> Result result -> RequestPath -> Hue result
put = request PUT

-- | Make a DELETE request
delete :: Result result -> RequestPath -> Hue result
delete = request DELETE NoBody


-- | Indicator for the body of a request.
data Body b where
  -- | Send a request with an empty body.
  NoBody :: Body ()
  -- | Send b as JSON body.
  Body :: ToJSON b => b -> Body b

-- | Send a request with an empty body.
noBody :: Body ()
noBody = NoBody

-- | Send 'a' as JSON body.
body :: ToJSON a => a -> Body a
body = Body

-- | Indicator for what to do with the data that comes back from a request.
data Result r where
  -- | Ignore the result. The response body only be checked for errors.
  IgnoreResult :: Result ()
  -- | Parse the response body as JSON.
  ParseResult :: FromJSON r => Result r

-- | Ignore the result. The response body only be checked for errors.
ignoreResult :: Result ()
ignoreResult = IgnoreResult

-- | Parse the response body as JSON.
parseResult :: FromJSON a => Result a
parseResult = ParseResult


-- | Construct a request from scratch.
-- 
-- This function gives you the opportunity to talk directly to any bridge endpoint
-- and provide or get data in your own types.
-- 
-- Pre-defined requests can be found in the module that they functionally belong to, for 
-- example see 'Hue.Light.lights'.
request :: StdMethod -> Body body -> Result result -> RequestPath -> Hue result
request method b result rPath = case result of
  IgnoreResult -> do
    _ :: [HueSuccess Value] <- mkRequest >>= performRequest
    pure ()
  ParseResult -> mkRequest >>= performRequest

  where
    mkRequest :: Hue HTTP.Request
    mkRequest = do
      path <- requestPath rPath . unCredentials . configCredentials <$> ask
      ip <- ipAddress . configIP <$> ask
      pure 
        $ setRequestPath path
        $ setRequestMethod (fromString $ show method)
        $ setRequestHost ip
        $ case b of
          (Body b') -> setRequestBodyJSON b' defaultRequest
          _        -> defaultRequest

    performRequest :: FromJSON a
                   => HTTP.Request
                   -> Hue a
    performRequest req = do
      response <- liftIO $ httpLBS req
      let responseText = toStrict $ getResponseBody response
      case eitherDecodeStrict' responseText of 
        Left err -> throwParseError err responseText
        Right (HueErrorResponse err) -> throwError $ HueApiException err
        Right (HueResponse a) -> pure a

    throwParseError :: String 
                    -> ByteString 
                    -> Hue a
    throwParseError err responseText = 
      liftIO $ throw $ Hue.Internal.JSONParseException $
          "Error parsing bridge response: \n\t"
          `append` Text.pack err
          `append` "\nAttempted to parse response:\n\t"
          `append` decodeUtf8 responseText


-- | Identifies the endpoint path for a 'Request'.
data RequestPath = RequestPath [PathSegment]

-- | The base path for most useful endpoints:
-- 
-- @/api@
api :: RequestPath
api = root /: "api"

-- | The root API endpoint.
-- 
-- This serves as base to construct all other endpoints from.
root :: RequestPath
root = RequestPath []


-- | A smaller part of a request path.
data PathSegment =
    TextSegment Text
  | CredentialsSegment

-- | Append this segment to a request path if credentials are to be inserted at that place.
credentials :: PathSegment
credentials = CredentialsSegment

-- | Append a textual segment to a request path.
-- This function exists to avoid ambiguities with OverloadedStrings.
(/:) :: RequestPath
      -> Text 
      -> RequestPath
(/:) = (/~)

-- | Append a segment to a request path
(/~) :: (ToPathSegment s) 
      => RequestPath
      -> s 
      -> RequestPath
RequestPath prev /~ segment = RequestPath (toSegment segment:prev)

-- | Return the path that the request currently represents.
-- 
-- URL-encodes each segment.
requestPath :: RequestPath
             -> Text -- ^ Credentials needed to query most endpoints.
             -> ByteString
requestPath (RequestPath segments) creds
  = toByteString 
  $ encodePathSegments 
  $ segmentToText creds
  <$> reverse segments
  where
    segmentToText _ (TextSegment s) = s
    segmentToText c CredentialsSegment = c

-- | Class for all things that can be turned into a PathSegment.
-- Anything that has a 'Text'ual representation could have an instance.
-- When implementing an instance of this class, you should probably first 
-- convert your type to 'Text' and call 'toSegment' on that.
-- 
-- Any textual segment is URL-encoded before a request is performed.
class ToPathSegment a where
  -- | Encode a datatype in an API path segment. 
  toSegment :: a -> PathSegment

-- | Identity
instance ToPathSegment PathSegment where
  toSegment = id

-- | Any 'Text' can be converted to an PathSegment.
instance ToPathSegment Text where
  toSegment = TextSegment
  

-- | Credentials can be used as part of an API path.
instance ToPathSegment HueCredentials where
  toSegment (HueCredentials creds) = TextSegment creds