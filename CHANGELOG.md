# Version 2.0.0

* Add Graph API version parameter. Avoid hardcoded `v2.8`.
* Add appsecret_proof verification.

# Version 1.2.1

* Make it work for ghc-8.4. See [#3](https://github.com/psibi/fb/issues/3)

# Version 1.2.0

* Rewrote fb for conduit-1.3.0
* Fixed various warnings and did general cleanup
* Fixed fetchNextPage/fetchPreviousPage test by adding ("filter", "stream") for comments api.
* Made lower bound of http-conduit to 2.3.0

# Version 1.1.1

* Make versioned call. By default now it uses `v2.8.
* `disassociateTestuser` function added.
* `getPage_` function added which accepts `AppAccessToken` as opposed
  to `getPage` function.
* Fixed a bug in `getObjectBool`
* Travis CI added
