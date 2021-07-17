# In case the configuration needs to substitute a dependency, we
# pass in the whole Nixpkgs value.

{ pkgs, version }:

with pkgs;

stdenv.mkDerivation {
  inherit version;

  name = "emacs-config-${version}";

  src = ./emacs.d;

  buildPhase = ''
    for elisp in $(find . -type f -name '*.el' -print); do
      echo ">>>>> replacing Nix outputs in ELisp file '$elisp'"
      substituteInPlace $elisp \
        --subst-var out
    done

    substituteInPlace packages.el \
      --subst-var-by sbcl ${pkgs.sbcl} 
  '';

  installPhase = ''
    mkdir -p $out
    cp -r ./* $out
  '';
}
