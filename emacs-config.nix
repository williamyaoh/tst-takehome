# In case the configuration needs to substitute a dependency, we
# pass in the whole Nixpkgs value.

{ pkgs, version, gitignoreSource }:

let
  jre = pkgs.openjdk11;
  sbt = pkgs.sbt.override (prev: prev // { inherit jre; });
  metals = pkgs.metals.override (prev: prev // { inherit jre; });
in

with pkgs;

stdenv.mkDerivation {
  inherit version;

  name = "emacs-config-${version}";

  src = gitignoreSource ./emacs.d;

  buildPhase = ''
    for elisp in $(find . -type f -name '*.el' -print); do
      echo ">>>>> replacing Nix outputs in ELisp file '$elisp'"
      substituteInPlace $elisp \
        --subst-var out
    done

    substituteInPlace packages.el \
      --subst-var-by ledger ${pkgs.ledger} \
      --subst-var-by sbcl ${pkgs.sbcl} \
      --subst-var-by git ${pkgs.git} \
      --subst-var-by rg ${pkgs.ripgrep} \
      --subst-var-by jre ${jre} \
      --subst-var-by sbt ${sbt} \
      --subst-var-by metals ${metals}
  '';

  installPhase = ''
    mkdir -p $out
    cp -r ./* $out
  '';
}
