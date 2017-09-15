-- |
-- Module: Hue.Internal.Light 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types to build representations for Hue API endpoints. 
module Hue.Internal.Endpoint (
  module Hue.Internal.Endpoint
, module MethodTypes
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Blaze.ByteString.Builder (toByteString)
import Network.HTTP.Types.URI (encodePathSegments)
import Network.HTTP.Types.Method as MethodTypes (StdMethod(..))

-- | The root API endpoint
root :: Endpoint method body result
root = Endpoint []

-- | Type repersenting a bridge API endpoint that can be queried.
-- 
-- And endpoint can be built by appending an 'EndpointSegment' to an existing Endpoint 
-- with either '/:' or '/~'.
-- 
-- Each endpoint must have a type annotation specifying the method, request body and
-- return type. '()' can be used for empty request body and response types.
--
-- Example:
-- 
-- @
-- lightsEndpoint :: Endpoint 'GET () [Lights]
-- lightsEndpoint = root /: "api" /~ credentials /: "lights"
-- @
data Endpoint (method :: StdMethod) body result = Endpoint [EndpointSegment]

-- | A smaller part of an endpoint.
data EndpointSegment =
    TextSegment Text
  | CredentialsSegment

-- | Append this segment to an endpoint if credentials are to be inserted at that place.
credentials :: EndpointSegment
credentials = CredentialsSegment

-- | Append a textual segment to an API endpoint.
-- This function exists to avoid ambiguities with OverloadedStrings.
(/:) :: Endpoint a b c
      -> Text 
      -> Endpoint method body d
(/:) = (/~)

-- | Append a segment to an API endpoint
(/~) :: (ToEndpointSegment s) 
      => Endpoint a b c
      -> s 
      -> Endpoint method body d
Endpoint prev /~ segment = Endpoint (toSegment segment:prev)

-- | Return the path that the endpoint currently represents.
endpointPath :: Endpoint a b c
             -> Text
             -> ByteString
endpointPath (Endpoint segments) creds
  = toByteString 
  $ encodePathSegments 
  $ segmentToText creds
  <$> reverse segments
  where
    segmentToText _ (TextSegment s) = s
    segmentToText c CredentialsSegment = c

-- | Class for all things that can be turned into an endpoint segment.
-- Anything that has a 'Text'ual representation could have an instance.
class ToEndpointSegment a where
  toSegment :: a -> EndpointSegment

-- | identity
instance ToEndpointSegment EndpointSegment where
  toSegment = id

-- | Any 'Text' can be converted to an EndpointSegment.
instance ToEndpointSegment Text where
  toSegment = TextSegment
  
