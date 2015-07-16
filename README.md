# Ankka

## Setting up the game

- Install necessary dependencies: `sudo apt-get install libglu1-mesa-dev libxrandr-dev libxcursor-dev libxinerama-dev libxi-dev mesa-common-dev libx11-dev libgl1-mesa-dev libftgl-dev cpphs`.
- Install Cabal + GHC `sudo apt-get install -y cabal-install-1.18 ghc-7.8.4`. Recommended GHC version is 7.8.x. Recommended version of Cabal is 1.18.0.x (otherwise you may not be able to get ghc-mod to work). You may also install these as part of `haskell-platform` (which includes many other things as well -- but it's harder to specify particular versions this way).
- Clone the repository.
- `cabal update` (just to make sure you have the latest package information)
- `cabal sandbox init` (create a local sandbox for all deps, instead of using a global)
- `cabal install --only-dependencies`
- `cabal configure`

## Running the game

- `cabal run`
