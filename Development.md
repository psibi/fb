# Running Tests

```
stack test
```

Right now, it results in 12 failures. These 12 failure happen because
our API don't have sufficient privilege. When the library was
originally developed, it passed all the tests. Facebook has been
introducing more permissions - so the tests don't pass completely
now. If anybody has a good idea of solving them, please open a issue
(or better send a PR!).

Result output:

``` shellsession
$ stack test
Stack has not been tested with GHC versions above 8.6, and using 8.8.2, this may fail
Stack has not been tested with Cabal versions above 2.4, but version 3.0.1.0 was found, this may fail
fb> test (suite: runtests)


Production tier: getAppAccessToken
  works and returns a valid app access token
  throws a FacebookException on invalid credentials
Production tier: setApiVersion
  Check default apiVersion
  Change apiVersion
Production tier: isValid
  returns False on a clearly invalid user access token
  returns False on a clearly invalid app access token
Production tier: debugToken
  works on a test user access token
Production tier: getObject
  is able to fetch Facebook's own page FAILED [1]
Production tier: getPage
  works for FB Developers FAILED [2]
Production tier: listSubscriptions
  returns something
Production tier: fetchNextPage
  seems to work on a public list of comments FAILED [3]
  seems to work on a private list of app insights
Production tier: fetchNextPage/fetchPreviousPage
  seems to work on a public list of comments FAILED [4]
  seems to work on a private list of app insights
Production tier: fetchAllNextPages
  seems to work on a public list of comments FAILED [5]
  seems to work on a private list of app insights
Production tier: createTestUser/removeTestUser/getTestUser
  creates and removes a new test user
Production tier: makeFriendConn
  creates two new test users, makes them friends and deletes them FAILED [6]
Production tier: getTestUsers
  gets a list of test users
Beta tier: getAppAccessToken
  works and returns a valid app access token
  throws a FacebookException on invalid credentials
Beta tier: setApiVersion
  Check default apiVersion
  Change apiVersion
Beta tier: isValid
  returns False on a clearly invalid user access token
  returns False on a clearly invalid app access token
Beta tier: debugToken
  works on a test user access token
Beta tier: getObject
  is able to fetch Facebook's own page FAILED [7]
Beta tier: getPage
  works for FB Developers FAILED [8]
Beta tier: listSubscriptions
  returns something
Beta tier: fetchNextPage
  seems to work on a public list of comments FAILED [9]
  seems to work on a private list of app insights
Beta tier: fetchNextPage/fetchPreviousPage
  seems to work on a public list of comments FAILED [10]
  seems to work on a private list of app insights
Beta tier: fetchAllNextPages
  seems to work on a public list of comments FAILED [11]
  seems to work on a private list of app insights
Beta tier: createTestUser/removeTestUser/getTestUser
  creates and removes a new test user
Beta tier: makeFriendConn
  creates two new test users, makes them friends and deletes them FAILED [12]
Beta tier: getTestUsers
  gets a list of test users
SimpleType
  works for Bool
  works for Day
  works for UTCTime
  works for ZonedTime
  works for Float
    +++ OK, passed 100 tests.
  works for Double
    +++ OK, passed 100 tests.
  works for Int
    +++ OK, passed 100 tests.
  works for Int8
    +++ OK, passed 100 tests.
  works for Int16
    +++ OK, passed 100 tests.
  works for Int32
    +++ OK, passed 100 tests.
  works for Int64
    +++ OK, passed 100 tests.
  works for Word
    +++ OK, passed 100 tests.
  works for Word8
    +++ OK, passed 100 tests.
  works for Word16
    +++ OK, passed 100 tests.
  works for Word32
    +++ OK, passed 100 tests.
  works for Word64
    +++ OK, passed 100 tests.
  works for [Float]
    +++ OK, passed 100 tests.
  works for [Double]
    +++ OK, passed 100 tests.
  works for [Int]
    +++ OK, passed 100 tests.
  works for [Int8]
    +++ OK, passed 100 tests.
  works for [Int16]
    +++ OK, passed 100 tests.
  works for [Int32]
    +++ OK, passed 100 tests.
  works for [Int64]
    +++ OK, passed 100 tests.
  works for [Word]
    +++ OK, passed 100 tests.
  works for [Word8]
    +++ OK, passed 100 tests.
  works for [Word16]
    +++ OK, passed 100 tests.
  works for [Word32]
    +++ OK, passed 100 tests.
  works for [Word64]
    +++ OK, passed 100 tests.
  works for Text
    +++ OK, passed 100 tests.
  works for Id
    +++ OK, passed 100 tests.
parseSignedRequest
  works for Facebook example
  fails to parse the Facebook example when signature is corrupted
addAppSecretProof
  appends appsecret_proof to the query when passing an access token
FQLTime
  seems to work
FbUTCTime
  seems to work (string)
  seems to work (unix epoch)
FQLList
  parses []
  parses {}
  parses [1234]
  parses {"1234": 1234}
FQLObject
  parses []
  parses {}
  parses {"abc": 1234}
  does not parse [1234]
Id
  can be parsed from a string
  can be parsed from an integer
  can be parsed from an object with a string
  can be parsed from an object with an integer
AccessToken
  can be round-tripped with ToJSON/FromJSON (UserKind)
  can be round-tripped with ToJSON/FromJSON (AppKind)
makeAppSecretProof
  generates correct hmac

Failures:

  tests/Main.hs:179:5:
  1) Production tier: getObject is able to fetch Facebook's own page
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature or the 'Page Public Metadata Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages, https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS and https://developers.facebook.com/docs/apps/review/feature#page-public-metadata-access for details."}

  To rerun use: --match "/Production tier: getObject/is able to fetch Facebook's own page/"

  tests/Main.hs:192:5:
  2) Production tier: getPage works for FB Developers
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature or the 'Page Public Metadata Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages, https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS and https://developers.facebook.com/docs/apps/review/feature#page-public-metadata-access for details."}

  To rerun use: --match "/Production tier: getPage/works for FB Developers/"

  tests/Main.hs:218:5:
  3) Production tier: fetchNextPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Production tier: fetchNextPage/seems to work on a public list of comments/"

  tests/Main.hs:244:5:
  4) Production tier: fetchNextPage/fetchPreviousPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Production tier: fetchNextPage/fetchPreviousPage/seems to work on a public list of comments/"

  tests/Main.hs:268:5:
  5) Production tier: fetchAllNextPages seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Production tier: fetchAllNextPages/seems to work on a public list of comments/"

  tests/Main.hs:320:5:
  6) Production tier: makeFriendConn creates two new test users, makes them friends and deletes them
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#33) This object does not exist or does not support this action"}

  To rerun use: --match "/Production tier: makeFriendConn/creates two new test users, makes them friends and deletes them/"

  tests/Main.hs:179:5:
  7) Beta tier: getObject is able to fetch Facebook's own page
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature or the 'Page Public Metadata Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages, https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS and https://developers.facebook.com/docs/apps/review/feature#page-public-metadata-access for details."}

  To rerun use: --match "/Beta tier: getObject/is able to fetch Facebook's own page/"

  tests/Main.hs:192:5:
  8) Beta tier: getPage works for FB Developers
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature or the 'Page Public Metadata Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages, https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS and https://developers.facebook.com/docs/apps/review/feature#page-public-metadata-access for details."}

  To rerun use: --match "/Beta tier: getPage/works for FB Developers/"

  tests/Main.hs:218:5:
  9) Beta tier: fetchNextPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Beta tier: fetchNextPage/seems to work on a public list of comments/"

  tests/Main.hs:244:5:
  10) Beta tier: fetchNextPage/fetchPreviousPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Beta tier: fetchNextPage/fetchPreviousPage/seems to work on a public list of comments/"

  tests/Main.hs:268:5:
  11) Beta tier: fetchAllNextPages seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) This endpoint requires the 'manage_pages' permission or the 'Page Public Content Access' feature. Refer to https://developers.facebook.com/docs/apps/review/login-permissions#manage-pages and https://developers.facebook.com/docs/apps/review/feature#reference-PAGES_ACCESS for details."}

  To rerun use: --match "/Beta tier: fetchAllNextPages/seems to work on a public list of comments/"

  tests/Main.hs:320:5:
  12) Beta tier: makeFriendConn creates two new test users, makes them friends and deletes them
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#33) This object does not exist or does not support this action"}

  To rerun use: --match "/Beta tier: makeFriendConn/creates two new test users, makes them friends and deletes them/"

Randomized with seed 1545350971

Finished in 99.2006 seconds
89 examples, 12 failures

fb> Test suite runtests failed
Test suite failure for package fb-2.1.0
    runtests:  exited with: ExitFailure 1
Logs printed to console
```

The `makeFriendConn` used to work before, but has started failing recently.
