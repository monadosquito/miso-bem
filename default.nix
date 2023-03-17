let
    pin = import ./chr/pin.nix;
in
    {
        pkgs ? (pin.miso {}).pkgs,
        useGhcjs ? false,
    }
    :
    let
        bem = pin.bem
                  {
                      inherit useGhcjs;
                  };
    in
        if useGhcjs
        then
            pkgs.haskell.packages.ghcjs.callCabal2nix
                "miso-bem"
                ./.
                {
                    inherit bem;
                }
        else
            pkgs.haskell.packages.ghc865.callCabal2nix
                "miso-bem"
                ./.
                {
                    inherit bem;
                }
