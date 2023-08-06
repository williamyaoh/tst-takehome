# This derivation is primarily responsible for ensuring that the
# correct packages are loaded into the Emacs installation.
# It is *not* responsible for configuring the built Emacs.

{ emacsWithPackages  # We let our caller specify the version here
}:

emacsWithPackages (epkgs: with epkgs; [
  # Because we expect our caller to be using Niv to pin dependencies,
  # we don't have to care about which specific ELPA-compatible repository
  # our Emacs packages are coming from.

  use-package  # incredibly important for configuration

  # Language support
  haskell-mode
  purescript-mode
  nix-mode
  elixir-mode
  erlang
  rust-mode
  typescript-mode
  terraform-mode
  ledger-mode
  markdown-mode
  yaml-mode

  paredit
  paren-face
  slime

  # General tools
  avy           # jump to char support
  magit
  discover      # contextual help menus when pressing <?>
  smex          # fuzzy M-x
  vlf
  yasnippet
  beacon        # highlight cursor on scroll
  undo-tree
  fill-column-indicator
  highlight-indentation

  # Theming
  github-modern-theme
  powerline
  ligature
])
