{
  inputs = {
      nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
      orgRepo = {
        url = "github:FixPlanet/org?rev=9fcdb024ba0cb11aec3b1f8469ec64658c19e2ca";
        flake = false;
      };
  };

  outputs = inputs:
    with inputs; let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [];
        });

    in {
      devShell = forAllSystems (system:
      nixpkgsFor.${system}.mkShell {
        packages = with nixpkgsFor.${system}; [
          imagemagick
          rsync
          (ghc.withPackages (ps: with ps; [
            MissingH
            aeson
            blaze-html
            clay
            hakyll
            pkgs.cabal-install
            tagsoup
            time-locale-compat
          ]))
        ];

        shellHook =
          let path = "project-selection-committee";
          in ''
            echo "Copying things from the org repo ..."
            # Committee-member markdown files
            rm -rf src/${path}
            mkdir src/${path}
            cp ${inputs.orgRepo.outPath}/${path}/*.md src/${path}

            # Committee-member mmages
            rm -rf src/images/${path}
            mkdir src/images/${path}
            cp ${inputs.orgRepo.outPath}/${path}/*.webp src/images/${path}

            # "Misc" org docs
            #
            # Note: Because of the way Hakyll works, we use pandoc (the tool) to
            # convert these to HTML so that we can include them in the website using
            # the $partial(...)$ directive.
            rm -rf src/org-docs
            mkdir src/org-docs

            # The "essential criteria" part of the document.
            pandoc ${inputs.orgRepo.outPath}/constitution/essential-criteria.md -o src/org-docs/essential-criteria.html
            echo "Done."
            '';
      }
      );
    };
}










# {
#   description = "Fix Planet Club";
  # inputs = {
  #     nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  #     metaRepo = {
  #       url = "git+ssh://git@github.com/FixPlanet/org?ref=main&rev=3a38e319c5d160a5117b9828023ce2205a19b017";
  #       flake = false;
  #     };
  # };

  #outputs = inputs:
  #  with inputs; let
  #    supportedSystems = [
  #      "aarch64-darwin"
  #      "aarch64-linux"
  #      "x86_64-darwin"
  #      "x86_64-linux"
  #    ];
  #    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
  #    nixpkgsFor = forAllSystems (system:
  #      import nixpkgs {
  #        inherit system;
  #        overlays = [];
  #      });

  #  in {
  #    devShell = forAllSystems (system:
  #    nixpkgsFor.${system}.mkShell {
  #      packages = with nixpkgsFor.${system}; [
  #        imagemagick
  #        rsync
  #        (ghc.withPackages (ps: with ps; [
  #          MissingH
  #          aeson
  #          blaze-html
  #          clay
  #          hakyll
  #          pkgs.cabal-install
  #          tagsoup
  #          time-locale-compat
  #        ]))
  #      ];

  #      shellHook =
  #        let path = "project-selection-committee";
  #        in ''
  #          echo "Copying things from the org repo ..."
  #          # Committee-member markdown files
  #          rm -rf src/${path}
  #          mkdir src/${path}
  #          cp ${org.outPath}/${path}/*.md src/${path}

  #          # Committee-member mmages
  #          rm -rf src/images/${path}
  #          mkdir src/images/${path}
  #          cp ${org.outPath}/${path}/*.{png,jpg} src/images/${path}

  #          # "Misc" org docs
  #          #
  #          # Note: Because of the way Hakyll works, we use pandoc (the tool) to
  #          # convert these to HTML so that we can include them in the website using
  #          # the $partial(...)$ directive.
  #          rm -rf src/org-docs
  #          mkdir src/org-docs

  #          # The "essential criteria" part of the document.
  #          pandoc ${org.outPath}/constitution/essential-criteria.md -o src/org-docs/essential-criteria.html
  #          echo "Done."
  #          '';
  #    }
  #    );
  #  };
#}










# {
#   description = "Fix Planet Club";

#   inputs = {
#     nixpkgs.url     = "github:nixos/nixpkgs/nixos-22.05";
#     flake-utils.url = "github:numtide/flake-utils";

#   };

#   outputs = inputs: with inputs;
#   flake-utils.lib.eachDefaultSystem (system:
#     let
#         pkgs = import nixpkgs {
#           inherit system;
#           # overlays = [];
#         };

#         org = pkgs.fetchgit {
#               url      = "https://github.com/FixPlanet/org";
#               fetchLFS = true;
#             };


#         hPkgs = pkgs.haskell.packages."ghc902";

#         stack-wrapped = pkgs.symlinkJoin {
#           name = "stack";
#           paths = [ pkgs.stack ];
#           buildInputs = [ pkgs.makeWrapper ];
#           postBuild = ''
#             wrapProgram $out/bin/stack \
#               --add-flags "\
#                 --no-nix \
#                 --system-ghc \
#                 --no-install-ghc \
#               "
#           '';
#         };

#         myDevTools = [
#           hPkgs.ghc
#           stack-wrapped
#           # External C libraries needed by some Haskell packages
#           pkgs.zlib
#           pkgs.zlib.dev
#           pkgs.pandoc
#         ];
#     in rec {
#       devShell = pkgs.mkShell {
#         buildInputs = [
#           myDevTools
#         ];


#         NIX_PATH = "nixpkgs=" + pkgs.path;
#         LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
#       };
#     }
#   );
# }
