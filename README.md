# web-push

[![Hackage](https://img.shields.io/hackage/v/web-push.svg)](https://hackage.haskell.org/package/web-push)
[![Spec and Mock Tests](https://github.com/cotrone/web-push/actions/workflows/web-push-tests.yml/badge.svg)](https://github.com/cotrone/web-push/actions/workflows/web-push-tests.yml)
[![Example Browser Tests](https://github.com/cotrone/web-push/actions/workflows/web-push-example.yml/badge.svg)](https://github.com/cotrone/web-push/actions/workflows/web-push-example.yml)

Send web push notifications to browsers

Forked from https://github.com/sarthakbagaria/web-push

## Usage

Guides to using Web Push API in browsers can be found on [Mozilla's](https://developer.mozilla.org/en/docs/Web/API/Push_API) and [Google's](https://developers.google.com/web/fundamentals/engage-and-retain/push-notifications/) docs, or you can check out the demo app in the example folder. To run the demo app:

with nix: `nix run .#web-push-example:exe:web-push-example-server`

with cabal: `cabal run web-push-example-server`

stack: `stack exec web-push-example-server`

Then access `localhost:3000` from a browser

For production use, store a set of VAPID keys securely and use them for all push notification subscriptions and messages; public key will have to be exposed to client's browser when subscribing to push notifications, but private key must be kept secret and used when generating push notifications on the server. If VAPID keys are re-generated, all push notifications will require re-subscriptions. Also save the latest subscription details such as endpoint from user's browser session securely in the database and use them to send push notifications to the user later.

## Generating Keys

Generating VAPID keys should be done with openssl.

To generate the private key:
```
openssl ecparam -name prime256v1 -genkey -noout -out vapid_private.pem
```

to calculate the public key from the private key:
```
openssl ec -in vapid_private.pem -pubout -out vapid_public.pem
```

## Build

The library builds and tests run with both cabal and stack.

There is also a nix flake setup to manage the dependencies for tests.

To build a cabal environment for the project that

### Nix

The nix flake includes:

- A development environment for web-push and web-push-example through `nix develop`
- A package for the example server `nix run .#web-push-example-server`
- A package for the mock web push server from https://github.com/marc1706/web-push-testing
  - `nix run .#web-push-testing -- start` and `nix run .#web-push-testing -- stop` to control it
  - Launch the server directly with `nix run .#web-push-testing-server -- 8090`
- Tests for `web-push` and `web-push-example`
  - For `web-push`: `nix run .#web-push-test`
  - For `web-push-example`: `nix run .#web-push-example-test`
- Browser tests for the `web-push-example` server with `nix run .#web-push-example-test`

## Example

There is a minimal example web application that generates keys, stores subscriptions, and sends notifications to all subscribers in [web-push-example](web-push-example/README.md)

## References

Current implementation is based on the following versions of the drafts:
- [https://tools.ietf.org/html/draft-ietf-webpush-encryption-04](https://tools.ietf.org/html/draft-ietf-webpush-encryption-04)
- [https://tools.ietf.org/html/draft-ietf-httpbis-encryption-encoding-02](https://tools.ietf.org/html/draft-ietf-httpbis-encryption-encoding-02)
- [https://tools.ietf.org/html/draft-ietf-webpush-protocol-10](https://tools.ietf.org/html/draft-ietf-webpush-protocol-10)
- [https://tools.ietf.org/html/draft-ietf-webpush-vapid-01](https://tools.ietf.org/html/draft-ietf-webpush-vapid-01)
