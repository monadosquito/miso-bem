let
    bem = pin.bem {};
    miso = pin.miso {};
    pin = import ./chr/pin.nix;
    traverse = pin.traverse {};
    unpath = pin.unpath {};
in
{
    ms ? miso,
    pkgs ? pin.nixpkgs {},
}
:
ms.pkgs.mkShell
    {
        buildInputs
            =
            [
                ms.pkgs.cabal-install
                traverse
                unpath
                (ms.pkgs.writeShellScriptBin
                     "watch"
                     (ms.pkgs.lib.readFile ./scr/watch.sh)
                )
                pkgs.ghcid
            ];
        inputsFrom
            =
                [
                    (ms.pkgs.haskell.packages.ghc865.callCabal2nix
                         "miso-bem"
                         ./.
                         {
                             inherit bem;
                         }
                    ).env
                ];
    }
