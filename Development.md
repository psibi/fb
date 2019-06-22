# Running Tests

```
stack test
```

Right now, it results in 10 failures. These 11 failure happen because
our API don't have sufficient privilege. When the library was
originally developed, it passed all the tests. Facebook has been
introducing more permissions - so the tests don't pass completely
now. If anybody has a good idea of solving them, please open a issue
(or better send a PR!).

Result output:

``` shellsession
$ stack test
fb-2.0.0: unregistering (local file changes: src/Facebook/Monad.hs)
fb> build (lib + test)
Preprocessing library for fb-2.0.0..
Building library for fb-2.0.0..
[ 3 of 17] Compiling Facebook.Monad
Preprocessing test suite 'runtests' for fb-2.0.0..
Building test suite 'runtests' for fb-2.0.0..
Linking .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/runtests/runtests ...
fb> copy/register
Installing library in /home/sibi/github/fb/.stack-work/install/x86_64-linux/6c0d2999eddfddb4dd99f5630b9297daeafc14550d37afa45f57dae4adad4475/8.6.5/lib/x86_64-linux-ghc-8.6.5/fb-2.0.0-9FsqbWJMwgU8QHDjw79gUA
Registering library for fb-2.0.0..
fb> test (suite: runtests)

Progress 1/2: fb
Production tier: getAppAccessToken
  works and returns a valid app access token
  throws a FacebookException on invalid credentials
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
^[^[  creates and removes a new test user
Production tier: makeFriendConn
  creates two new test users, makes them friends and deletes them
Production tier: getTestUsers
  gets a list of test users
Beta tier: getAppAccessToken
  works and returns a valid app access token
  throws a FacebookException on invalid credentials
Beta tier: isValid
  returns False on a clearly invalid user access token
  returns False on a clearly invalid app access token
Beta tier: debugToken
  works on a test user access token
Beta tier: getObject
  is able to fetch Facebook's own page FAILED [6]
Beta tier: getPage
  works for FB Developers FAILED [7]
Beta tier: listSubscriptions
  returns something
Beta tier: fetchNextPage
  seems to work on a public list of comments FAILED [8]
  seems to work on a private list of app insights
Beta tier: fetchNextPage/fetchPreviousPage
  seems to work on a public list of comments FAILED [9]
  seems to work on a private list of app insights
Beta tier: fetchAllNextPages
  seems to work on a public list of comments FAILED [10]
  seems to work on a private list of app insights
Beta tier: createTestUser/removeTestUser/getTestUser
  creates and removes a new test user
Beta tier: makeFriendConn
  creates two new test users, makes them friends and deletes them
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

  tests/Main.hs:175:5:
  1) Production tier: getObject is able to fetch Facebook's own page
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Production tier: getObject/is able to fetch Facebook's own page/"

  tests/Main.hs:188:5:
  2) Production tier: getPage works for FB Developers
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Production tier: getPage/works for FB Developers/"

  tests/Main.hs:214:5:
  3) Production tier: fetchNextPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Production tier: fetchNextPage/seems to work on a public list of comments/"

  tests/Main.hs:240:5:
  4) Production tier: fetchNextPage/fetchPreviousPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Production tier: fetchNextPage/fetchPreviousPage/seems to work on a public list of comments/"

  tests/Main.hs:264:5:
  5) Production tier: fetchAllNextPages seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Production tier: fetchAllNextPages/seems to work on a public list of comments/"

  tests/Main.hs:175:5:
  6) Beta tier: getObject is able to fetch Facebook's own page
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Beta tier: getObject/is able to fetch Facebook's own page/"

  tests/Main.hs:188:5:
  7) Beta tier: getPage works for FB Developers
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Beta tier: getPage/works for FB Developers/"

  tests/Main.hs:214:5:
  8) Beta tier: fetchNextPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Beta tier: fetchNextPage/seems to work on a public list of comments/"

  tests/Main.hs:240:5:
  9) Beta tier: fetchNextPage/fetchPreviousPage seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Beta tier: fetchNextPage/fetchPreviousPage/seems to work on a public list of comments/"

  tests/Main.hs:264:5:
  10) Beta tier: fetchAllNextPages seems to work on a public list of comments
       uncaught exception: FacebookException
       FacebookException {fbeType = "invalid_request", fbeMessage = "(#10) To use 'Page Public Content Access', your use of this endpoint must be reviewed and approved by Facebook. To submit this 'Page Public Content Access' feature for review please read our documentation on review}

  To rerun use: --match "/Beta tier: fetchAllNextPages/seems to work on a public list of comments/"

Randomized with seed 1487051425

Finished in 226.3622 seconds
85 examples, 10 failures

fb> Test suite runtests failed
Completed 2 action(s).
Test suite failure for package fb-2.0.0
    runtests:  exited with: ExitFailure 1
Logs printed to console
```
