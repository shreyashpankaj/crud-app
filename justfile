default:
  @just --list

# something
build:
  nix build

ghcid:
  ghcid -T :main