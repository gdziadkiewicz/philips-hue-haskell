module Hue.Endpoint (
  Endpoint
, api
, root 
, (/:)
, (/~)
, endpointPath
, EndpointSegment
, credentials
, ToEndpointSegment
) where 
  
import Hue.Internal.Endpoint