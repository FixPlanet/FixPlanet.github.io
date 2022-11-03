{
  description = "Fix Planet Club";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = inputs: with inputs;
  flake-utils.lib.eachDefaultSystem (system:
    let
        pkgs = import nixpkgs {
          inherit system;
          # overlays = [];
        };

        org = pkgs.fetchgit {
              url      = "https://github.com/FixPlanet/org";
              rev      = "3a38e319c5d160a5117b9828023ce2205a19b017";
              sha256   = "sha256-kykePQMJBuPuf2Z0opSRcEAoxqqti9ehzAmB+e9s888=";
              fetchLFS = true;
            };

        path = "project-selection-committee";

        hPkgs = pkgs.haskell.packages."ghc902";

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        myDevTools = [
          hPkgs.ghc
          stack-wrapped
          # External C libraries needed by some Haskell packages
          pkgs.zlib
          pkgs.pandoc
        ];
    in rec {
      devShell = pkgs.mkShell {
        buildInputs = [
          myDevTools
        ];

        shellHook = ''
          echo "Copying things from the org repo ..."
          # Committee-member markdown files
          rm -rf src/${path}
          mkdir src/${path}
          cp ${org.outPath}/${path}/*.md src/${path}

          # Committee-member mmages
          rm -rf src/images/${path}
          mkdir src/images/${path}
          cp ${org.outPath}/${path}/*.{png,jpg} src/images/${path}

          # "Misc" org docs
          #
          # Note: Because of the way Hakyll works, we use pandoc (the tool) to
          # convert these to HTML so that we can include them in the website using
          # the $partial(...)$ directive.
          rm -rf src/org-docs
          mkdir src/org-docs

          # The "essential criteria" part of the document.
          pandoc ${org.outPath}/constitution/essential-criteria.md -o src/org-docs/essential-criteria.html
          echo "Done."
          '';

        NIX_PATH = "nixpkgs=" + pkgs.path;
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
      };
    }
  );
}
