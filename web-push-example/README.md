# web-push-example

This is an example server that generates keys, manages subscriptions, and serves the minimal files to subscribe to push notifications from browsers and send push notifications from the server to a browser.

The server runs on `localhost:3000` and can test notifications to multiple browsers

## Tests

There are tests that use the example server in [test/Main.hs](test/Main.hs) with webdriver to open a browser.
Geckodriver currently breaks firefox so only the chrome test is ran https://github.com/mozilla/geckodriver/issues/1687
