module Hue.Endpoint (
  Endpoint
, root 
, (/:)
, (/~)
, endpointPath
, EndpointSegment
, credentials
, ToEndpointSegment
) where 
  
import Hue.Internal.Endpoint