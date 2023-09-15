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
      # Web push is a JS project designed to emulate the behaviour of browsers
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
      web-push-example-tests = pkgs.writeScriptBin "web-push-example-tests" ''
        trap "kill 0" EXIT
        echo "Starting geckodriver"
        ${pkgs.geckodriver}/bin/geckodriver &
        GECKODRIVER_PID=$!

        echo "Starting chromedriver"
        ${pkgs.chromedriver}/bin/chromedriver &
        CHROMEDRIVER_PID=$!

        echo "Starting web-push-example-tests"
        ${flake.packages."web-push-example:test:web-push-example-test"}/bin/web-push-example-test
        EXIT_CODE=$?

        exit $EXIT_CODE

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
        web-push-example-test = web-push-example-tests;
      };
    });
}
