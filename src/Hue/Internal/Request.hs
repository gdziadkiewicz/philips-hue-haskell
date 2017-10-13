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
{-# LANGUAGE GADTs, RankNTypes #-}
module Hue.Internal.Request (
  module Hue.Internal.Request
, module MethodTypes
) where

import Data.Aeson hiding (Result(..))
import Data.String (fromString)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (toByteString)
import Network.HTTP.Types.URI (encodePathSegments)
import Network.HTTP.Types.Method as MethodTypes (StdMethod(..))


-- | Type repersenting a request that can be sent to the bridge.
-- Requests identify:
-- 
--  * which API gets called,
-- 
--  * which request method is used,
-- 
--  * the body of the request,
-- 
--  * the result of the request.
-- 
-- Pre-defined requests can be found in the module that they functionally belong to, for 
-- example see 'lights' in "Hue.Light".
-- 
-- A request is built from:
-- 
--  * a request method ('get', 'post', 'put', or 'delete')
-- 
--  * a 'Body'
-- 
--  * an indicator for what to do with the response. See 'Result'.
-- 
--  * a 'RequestPath'
-- 
-- The 'RequestPath' can be built by appending an 'PathSegment' to an existing 
-- RequestPath with either '/:' or '/~'.
-- 
-- If you do not care about the data returned from the endpoint, you should use 'IgnoreResponse'.
-- 
-- Each request must have a type annotation specifying the request body and
-- return type. '()' can be used for empty request body and response types.
--
-- Example:
-- 
-- @
-- lightsRequest :: Request [Lights]
-- lightsRequest = get ParseResult $ api \/~ credentials /: "lights"
-- @
data Request r where
  Request :: StdMethod -> Body b -> Result r -> RequestPath -> Request r

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

-- | Make a GET request
get :: Result result -> RequestPath -> Request result
get = mkRequest GET NoBody

-- | Make a POST request
post :: Body body -> Result result -> RequestPath -> Request result
post = mkRequest POST

-- | Make a PUT request
put :: Body body -> Result result -> RequestPath -> Request result
put = mkRequest PUT

-- | Make a DELETE request
delete :: Result result -> RequestPath -> Request result
delete = mkRequest DELETE NoBody

-- | Construct a request for any HTTP method, body and result.
mkRequest :: StdMethod -> Body b -> Result r -> RequestPath -> Request r
mkRequest = Request

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
requestPath :: Request a 
             -> Text -- ^ Credentials needed to query most endpoints.
             -> ByteString
requestPath (Request _ _ _ (RequestPath segments)) creds
  = toByteString 
  $ encodePathSegments 
  $ segmentToText creds
  <$> reverse segments
  where
    segmentToText _ (TextSegment s) = s
    segmentToText c CredentialsSegment = c

-- | Get the method of a 'Request' as a ByteString.
requestMethod :: Request a -> ByteString
requestMethod (Request m _ _ _) = fromString $ show m

-- | Perform a computation based on the body of a request.
-- Since the body type is existential, you'll need to provide a function that's polymorphic in 'b'.
withRequestBody :: Request a -> (forall b. Body b -> x) -> x
withRequestBody (Request _ b _ _) f = f b

-- | Get how a response for this request is handled.
requestResult :: Request a -> Result a
requestResult (Request _ _ r _) = r

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
  
