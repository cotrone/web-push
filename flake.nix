{
  description = "Web push notifications";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.web-push-testing-src = {
    url = "github:cotrone/web-push-testing?ref=server-bin";
    flake = false;
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, web-push-testing-src }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
    let
      compiler-nix-name = "ghc928";
      # Web push is a JS project that emulates the behaviour of push notifications in browsers and the servers that deliver notifications to them
      web-push-testing = pkgs.buildNpmPackage {
        pname = "web-push-testing";
        version = "1.0.0";
        src = web-push-testing-src;
        npmDepsHash = "sha256-KPy4OImh9pDeXVx3zCZsw4low1SqLdWY87HHIdzRsRQ=";
        dontNpmBuild = true;
        buildInputs = [ pkgs.makeWrapper ];
        preConfigure = ''
          chmod +x src/bin/server.js
          ls -lah src/bin/server.js
          patchShebangs --build src/bin/server.js
        '';
        postFixup = "wrapProgram $out/bin/web-push-testing --prefix PATH : ${pkgs.nodejs}/bin";
        meta = with pkgs.lib; {
          description = "A server that can be run to test against a mocked API endpoint for web push without relying on flaky browser drivers.";
          homepage = "https://github.com/marc1706/web-push-testing";
          license = licenses.mit;
        };
      };
      overlays = [haskellNix.overlay (final: prev: {
        web-push =
          final.haskell-nix.cabalProject' {
            src = ./.;
            inherit compiler-nix-name;
            
            shell.tools = {
              cabal = {};
              ghcid = {};
              haskell-language-server = {}; # This doesn't work on the internal web-push-example project
            };
            shell.buildInputs = [ web-push-testing
                                  pkgs.firefox pkgs.geckodriver
                                  pkgs.google-chrome pkgs.chromedriver
                                  ];
          };
      })];
      # Wrap the web-push-test binary to include the web-push-testing-server binary in the PATH
      web-push-test = pkgs.symlinkJoin {
        name = "web-push-test";
        nativeBuildInputs = [ pkgs.makeWrapper ];
        paths = [ web-push-testing flake.packages."web-push:test:web-push-test" ];
        postBuild = ''
          wrapProgram $out/bin/web-push-test --prefix PATH : ${web-push-testing}/bin
        '';
      };
      web-push-example-test = pkgs.writeScriptBin "web-push-example-test" ''
        export CHROME_BINARY=${pkgs.google-chrome}/bin/google-chrome-stable
        export FIREFOX_BINARY=${pkgs.firefox}/bin/firefox

        echo "Starting geckodriver"
        ${pkgs.geckodriver}/bin/geckodriver --log error 2>&1 > /dev/null &
        GECKODRIVER_PID=$!

        echo "Starting chromedriver"
        ${pkgs.chromedriver}/bin/chromedriver --log-level=SEVERE 2>&1 > /dev/null &
        CHROMEDRIVER_PID=$!

        cleanup() {
          code=$?
          echo "Killing chromedriver and geckodriver and returning $code"
          kill $CHROMEDRIVER_PID
          kill $GECKODRIVER_PID
          exit $code
        }
        trap "cleanup" EXIT

        echo "Wait for geckodriver to start"
        timeout 30 sh -c 'until ${pkgs.netcat}/bin/nc -z $0 $1; do sleep 1; done' localhost 4444

        GECKODRIVER_EXIT_CODE=$?
        if [ $GECKODRIVER_EXIT_CODE -ne 0 ]; then
          echo "Failed to start geckodriver"
          exit $GECKODRIVER_EXIT_CODE
        fi

        timeout 30 sh -c 'until ${pkgs.netcat}/bin/nc -z $0 $1; do sleep 1; done' localhost 9515
        # Exit if either of the timeout fails
        CHROMEDRIVER_EXIT_CODE=$?
        if [ $CHROMEDRIVER_EXIT_CODE -ne 0 ]; then
          echo "Failed to start chromedriver"
          exit $CHROMEDRIVER_EXIT_CODE
        fi

        echo "Starting web-push-example-tests"
        ${flake.packages."web-push-example:test:web-push-example-test"}/bin/web-push-example-test
        EXIT_CODE=$?
        echo "web-push-example-tests exited with code $EXIT_CODE"
        exit $EXIT_CODE
      '';
      # Populate the ci cache with the binaries for tests
      # this requires the cache keys to be set in the environment
      populate-ci-cache = pkgs.writeScriptBin "populate-ci-cache" ''
        nix flake archive --json | ${pkgs.jq}/bin/jq -r '.path,(.inputs|to_entries[].value.path)' | ${pkgs.cachix}/bin/cachix push web-push
        nix build .#web-push-test --json | ${pkgs.jq}/bin/jq -r '.[].outputs | to_entries[].value' | ${pkgs.cachix}/bin/cachix push web-push
        nix build .#web-push-example-test --json | ${pkgs.jq}/bin/jq -r '.[].outputs | to_entries[].value' | ${pkgs.cachix}/bin/cachix push web-push
      '';
      pkgs = import nixpkgs {
        inherit system overlays;
        config = haskellNix.config // {
          allowUnfree = true;
        };
      };
      flake = pkgs.web-push.flake { } ;
    in flake // {
      packages = flake.packages // {
        web-push-testing = web-push-testing;
        web-push-example-server = flake.packages."web-push-example:exe:web-push-example-server";
        web-push-example-test = web-push-example-test;
        web-push-test = web-push-test;
        populate-ci-cache = populate-ci-cache;
      };
    });
}
