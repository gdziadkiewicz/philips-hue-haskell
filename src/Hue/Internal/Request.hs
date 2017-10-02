-- |
-- Module: Hue.Internal.Request 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types to build representations for Hue API requests. 
module Hue.Internal.Request (
  module Hue.Internal.Request
, module MethodTypes
) where

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
--  * a 'RequestPath'
-- 
-- The 'RequestPath' can be built by appending an 'PathSegment' to an existing 
-- RequestPath with either '/:' or '/~'.
-- 
-- Each request must have a type annotation specifying the request body and
-- return type. '()' can be used for empty request body and response types.
--
-- Example:
-- 
-- @
-- lightsRequest :: Request () [Lights]
-- lightsRequest = get $ api \/~ credentials /: "lights"
-- @
data Request body result = Request StdMethod RequestPath

-- | Make a GET request
get :: RequestPath -> Request () result
get = Request GET

-- | Make a POST request
post :: RequestPath -> Request body result
post = Request POST

-- | Make a PUT request
put :: RequestPath -> Request body result
put = Request PUT

-- | Make a DELETE request
delete :: RequestPath -> Request () result
delete = Request DELETE

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
requestPath :: Request x y
             -> Text
             -> ByteString
requestPath (Request _ (RequestPath segments)) creds
  = toByteString 
  $ encodePathSegments 
  $ segmentToText creds
  <$> reverse segments
  where
    segmentToText _ (TextSegment s) = s
    segmentToText c CredentialsSegment = c

-- | Get the method of a 'Request' as a ByteString.
requestMethod :: Request x y -> ByteString
requestMethod (Request m _) = fromString $ show m
  
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
  
