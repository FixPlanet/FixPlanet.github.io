{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

let org = pkgs.fetchgit {
            url      = "https://github.com/FixPlanet/org";
            rev      = "4507755ee4ce43e279995b21b384aef338ee81f3";
            sha256   = "sha256-cQzjlrSzu8m8PsbOe/+YVRqc1y0SR4NlfJZ7KKqqyqU=";
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
