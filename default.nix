let
  version = "0.1.0";

  sources = import ./nix/sources.nix {};

  nixpkgs = import sources.nixpkgs {};
in

rec {
  emacs-raw = import ./emacs-raw.nix {
    emacsWithPackages = nixpkgs.emacs26WithPackages;
  };

  emacs-config = import ./emacs-config.nix {
    inherit version;
    pkgs = nixpkgs;
  };

  emacs-packaged = nixpkgs.stdenv.mkDerivation rec {
    inherit version;

    name = "emacs-packaged-${version}";
    src = ./.;  # doesn't really matter, but forces a hash change

    buildInputs = [
      nixpkgs.makeWrapper
    ];

    BUILD_DIR = "_build";

    buildPhase = ''
      mkdir -p ${BUILD_DIR}
      makeWrapper ${emacs-raw}/bin/emacs ${BUILD_DIR}/emacs \
        --argv0 emacs \
        --add-flags "-q --load ${emacs-config}/init.el"
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp ${BUILD_DIR}/emacs $out/bin/emacs
    '';
  };

  emacs = emacs-packaged;
}
