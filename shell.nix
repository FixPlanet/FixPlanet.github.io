{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = pkgs.fetchgit {
            url      = "https://github.com/FixPlanet/org";
            rev      = "2d420929f6934c03f331f07ce72bfcab688a085d";
            sha256   = "sha256-WsWQtmWktUrxZQVMMmQ/jEaR854xPh1muWRCSFEBm7g=";
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
