{ system ? builtins.currentSystem
, config ? {}
, walletPackages ? import ./.. { inherit config system; }
, pkgs ? walletPackages.pkgs
, buildTools ? with pkgs; [ git nix gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath ([ walletPackages.iohkLib.cache-s3 stack gnused coreutils ] ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''
