# scotty-resource

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

```haskell
import Web.Scotty.Trans (get, scottyT, text)

...

scottyT 8080 id $ do
  get "/hello" $ do
    text "world"
```

If a client requests something like `DELETE /hello`, this scotty application
will return `404 Not Found`, which conflicts with section 5.1.1 of rfc-2616.
A better response would be `405 Method Not Allowed`, and it would include an
automatically generated `Allow` response header.

This library gives users a way to model "resources" which is closer to the
abstractions used in the HTTP standard.

We can re-write the above example like this:

```haskell
import Web.Scotty.Trans (scottyT, text)
import Web.Scotty.Resource.Trans (resource, get)

...

scottyT 8080 id $ do
  resource "/hello" $ do
    get $ do
      text "world"
```

Given a request:

```http
DELETE /hello HTTP/1.1
Host: localhost:8080
```

The resource-based scotty application will produce something like:

```http
HTTP/1.1 405 Method Not Allowed
Allow: GET
```

Each resource is described by a `WebResource` value, which happens to be a
`Monad`. The only reason `WebResource` implements `Monad` to fit in with the
do-notation coding style of `ScottyT`. This is an abuse of `Monad`, but, you
know, whatever. The `Monoid` typeclass more correctly represents what a
`WebResource` really is. The `Monad` and `Monoid` typeclasses are used to
compose instances of `WebResource`.

Here is another more complex example, with multiple resources.

```haskell
import Data.Aeson (decode)
import Network.HTTP.Types (notFound404, badRequest400, noContent204)
import Web.Scotty.Resource.Trans (resource, get, post)
import Web.Scotty.Trans (scottyT, text, body, raw, status, param)

import MyApplication (lookupPerson, storePerson)

...

scottyT 8080 id $ do

  -- an "echo" resource
  resource "/echo" $ do
    get $ do
      text "hello world"
    post $ do
      -- echo the request body back to the user
      raw =<< body

  -- A resource that represents a kind of a RESTful database of people.
  -- This resource supports GET and PUT.
  resource "/people/:personId" $ do
    get $ do
      personId <- param "personId"
      maybePerson <- lookupPerson personId
      case maybePerson of
        Nothing ->
          status notFound404
        Just person ->
          json person
    put $ do
      personId <- param "personId"
      maybePerson <- decode <$> body
      case maybePerson of
        Nothing -> do
          status badRequest400
          text "Invalid person JSON"
        Just person ->
          storePerson personId person
          status noContent204
```

