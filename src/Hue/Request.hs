-- |
-- Module: Hue.Request 
-- Copyright: (c) 2017 Thomas Smith
-- License: BSD3
-- Maintainer: Thomas Smith <tnsmith@live.nl>
-- Stability: experimental
--
-- Types to build representations for Hue API requests. 
module Hue.Request (
-- * Constructing requests
  get
, post
, put
, delete
, request
-- * Adding a body to the Request
, Body
, body
, noBody
-- * Indicating the result type of a Request
, Result
, ignoreResult
, parseResult
-- * Constructing the API path
, RequestPath
, api
, root 
, (/:)
, (/~)
-- ** Adding custom data to the request path
, PathSegment
, credentials
, ToPathSegment
, toSegment
-- * Inspecting a Request
, requestPath
) where 
  
import Hue.Internal.Request