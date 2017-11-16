#!/usr/bin/env sh

export NIX_PATH="nixpkgs=$HOME/github/nixos/nixpkgs"
export NIX_PATH="$NIX_PATH:nixos-config=$PWD/config.nix"
export NIX_PATH="$NIX_PATH:nixpkgs-overlays=$PWD/overlays"
options=$(nix-build '<nixpkgs/nixos/release.nix>' -A options --no-out-link)
jq . < "$options/share/doc/nixos/options.json"

nix-instantiate --eval -E '(import <nixpkgs/nixos> {}).options.services.xserver.enable.type'
nix-instantiate --eval -E '(import <nixpkgs/nixos> {}).config.services.xserver.enable'
nix-instantiate --eval -E '(import <nixpkgs/nixos> { configuration = {}; }).config.services.xserver.enable'
