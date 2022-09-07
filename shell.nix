{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = pkgs.fetchgit {
            url      = "https://github.com/FixPlanet/org";
            rev      = "c4b1e9d3f2ad08984fb1031d012ed142ba975a47";
            sha256   = "sha256-jsHRBLF3UyUa/ftPvgJ9ajme5iJkQd26vvWVqz0W0fU=";
            fetchLFS = true;
          };
    path = "project-selection-committee";
 in

mkShell {
  buildInputs = [ ];
  shellHook = ''
    # Markdown files
    rm -rf src/${path}
    mkdir src/${path}
    cp ${org.outPath}/${path}/*.md src/${path}

    # Images
    rm -rf src/images/${path}
    mkdir src/images/${path}
    cp ${org.outPath}/${path}/*.{png,jpg} src/images/${path}
  '';
}
