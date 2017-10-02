-- |
-- Module: Hue.Request 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types to build representations for Hue API requests. 
module Hue.Request (
  Request
, get
, post
, put
, delete
, RequestPath
, api
, root 
, (/:)
, (/~)
, requestPath
, requestMethod
, PathSegment
, credentials
, ToPathSegment
, toSegment
) where 
  
import Hue.Internal.Request