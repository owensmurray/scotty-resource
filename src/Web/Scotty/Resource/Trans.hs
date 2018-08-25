{-# LANGUAGE OverloadedStrings #-}
{- |
  This module defines better resource routing for Scotty.

  Scotty is defined in terms of "routes", whereas HTTP is defined in
  terms of "resources".  This package adds a "resource" abstraction to
  the scotty ecosystem.

  (note: All examples probably require -XOverloadedStrings)

  Scotty comes out of the box with a way to model "routes". The problem
  is that "routes" is not the abstraction used by the HTTP standard and
  it can sometimes be tricky to write a perfectly correct HTTP service
  using the routes model (where "correct" is judged against rfc-2616).
  The most blatant, (and who knows, maybe the only) example of this problem
  is shown by the scotty code:

  > import Web.Scotty.Trans (get, scottyT, text)
  >
  > ...
  >
  > scottyT 8080 id $ do
  >   get "/hello" $ do
  >     text "world"

  If a client requests something like @DELETE /hello@, this scotty application
  will return @404 Not Found@, which conflicts with section 5.1.1 of rfc-2616.
  A better response would be @405 Method Not Allowed@, and it would include an
  automatically generated @Allow@ response header.

  This library gives users a way to model "resources" which is closer to the
  abstractions used in the HTTP standard.

  We can re-write the above example like this:

  > import Web.Scotty.Trans (scottyT, text)
  > import Web.Scotty.Resource.Trans (resource, get)
  >
  > ...
  >
  > scottyT 8080 id $ do
  >   resource "/hello" $ do
  >     get $ do
  >       text "world"

  Given a request:

  > DELETE /hello HTTP/1.1
  > Host: localhost:8080

  The resource-based scotty application will produce something like:

  > HTTP/1.1 405 Method Not Allowed
  > Allow: GET

  Each resource is described by a `WebResource` value, which happens to
  be a `Monad`. The only reason `WebResource` implements `Monad` to fit
  in with the do-notation coding style of `ScottyT`. This is an abuse of
  `Monad`, but, you know, whatever. The `Semigroup` typeclass more correctly
  represents what a `WebResource` really is. The `Monad` and `Semigroup`
  typeclasses are used to compose instances of `WebResource`.

  Here is another more complex example, with multiple resources.

  > import Data.Aeson (decode)
  > import Network.HTTP.Types (notFound404, badRequest400, noContent204)
  > import Web.Scotty.Resource.Trans (resource, get, post)
  > import Web.Scotty.Trans (scottyT, text, body, raw, status, param)
  >
  > import MyApplication (lookupPerson, storePerson)
  > 
  > ...
  > 
  > scottyT 8080 id $ do
  >
  >   -- an "echo" resource
  >   resource "/echo" $ do
  >     get $ do
  >       text "hello world"
  >     post $ do
  >       -- echo the request body back to the user
  >       raw =<< body
  >
  >   -- A resource that represents a kind of a RESTful database of people.
  >   -- This resource supports GET and PUT.
  >   resource "/people/:personId" $ do
  >     get $ do
  >       personId <- param "personId"
  >       maybePerson <- lookupPerson personId
  >       case maybePerson of
  >         Nothing ->
  >           status notFound404
  >         Just person ->
  >           json person
  >     put $ do
  >       personId <- param "personId"
  >       maybePerson <- decode <$> body
  >       case maybePerson of
  >         Nothing -> do
  >           status badRequest400
  >           text "Invalid person JSON"
  >         Just person ->
  >           storePerson personId person
  >           status noContent204
-}
module Web.Scotty.Resource.Trans (
  resource,
  WebResource,
  options,
  get,
  head,
  post,
  put,
  delete,
  patch,
  method
) where

import Prelude hiding (head)

import Control.Monad (liftM, ap)
import Control.Monad.IO.Class (MonadIO)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Network.HTTP.Types (Method, methodNotAllowed405)
import Network.Wai (requestMethod)
import Web.Scotty.Trans (ActionT, RoutePattern, ScottyT, matchAny,
  request, ScottyError, setHeader, status)

{- |
  Add a resource whose uri matches the route pattern.
-}
resource :: (MonadIO m, ScottyError e)
  => RoutePattern
  -> WebResource e m ()
  -> ScottyT e m ()
resource uri (W methods ()) = matchAny uri $
    getMethod `liftM` request >>= fromMaybe notAllowed
  where
    getMethod = (`lookup` methods) . requestMethod
    notAllowed = do
      setHeader "Allow" allowed
      status methodNotAllowed405

    {-
      Build a list of allowed methods.
    -}
    allowed =
      (
        fromStrict
        . decodeUtf8
        . mconcat
        . intersperse ", "
        . dedupe
        . fmap fst
      ) methods


{- |
  An opaque representation of an http resource. Use `get`, `post`, etc. to
  create one of these. Use the `Monad` or `Semigroup` instances to compose them.
-}
data WebResource e m a = W [(Method, ActionT e m ())] a
instance Functor (WebResource e m) where
  fmap = liftM
instance Applicative (WebResource e m) where
  pure = return
  (<*>) = ap
instance Monad (WebResource e m) where
  return = W []
  W methods a >>= f =
    let W newMethods b = f a
    in W (methods <> newMethods) b
instance Semigroup (WebResource e m a) where
  W a _ <> W b c = W (a <> b) c


{- |
  Create a `WebResource` that handles OPTIONS requests using the given
  scotty action.
-}
options :: ActionT e m () -> WebResource e m ()
options action = W [("OPTIONS", action)] ()


{- |
  Create a `WebResource` that handles GET requests using the given
  scotty action.
-}
get :: ActionT e m () -> WebResource e m ()
get action = W [("GET", action)] ()


{- |
  Create a `WebResource` that handles HEAD requests using the given
  scotty action.
-}
head :: ActionT e m () -> WebResource e m ()
head action = W [("HEAD", action)] ()


{- |
  Create a `WebResource` that handles POST requests using the given
  scotty action.
-}
post :: ActionT e m () -> WebResource e m ()
post action = W [("POST", action)] ()


{- |
  Create a `WebResource` that handles PUT requests using the given
  scotty action.
-}
put :: ActionT e m () -> WebResource e m ()
put action = W [("PUT", action)] ()


{- |
  Create a `WebResource` that handles DELETE requests using the given
  scotty action.
-}
delete :: ActionT e m () -> WebResource e m ()
delete action = W [("DELETE", action)] ()


{- |
  Create a `WebResource` that handles PATCH requests using the given
  scotty action.
-}
patch :: ActionT e m () -> WebResource e m ()
patch action = W [("PATCH", action)] ()


{- |
  Create a `WebResource` that handles the specific method using the given
  scotty action.
-}
method :: Method -> ActionT e m () -> WebResource e m ()
method m action = W [(m, action)] ()


{- |
  A helper function that removes duplicates from a list.
-}
dedupe :: (Ord a) => [a] -> [a]
dedupe = toList . fromList


