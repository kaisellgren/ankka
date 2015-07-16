# ankka

## Setting up the game

- `sudo apt-get install libglu1-mesa-dev libxrandr-dev libxcursor-dev libxinerama-dev libxi-dev mesa-common-dev libx11-dev libgl1-mesa-dev`
- git clone
- Install the Haskell platform along with the cabal package manager. Recommended GHC version is 7.8.x. Recommended version of Cabal is 1.18.0.x (otherwise you may not be able to get ghc-mod to work). `sudo apt-get install -y cabal-install-1.18 ghc-7.8.4`
- cabal update (just to make sure you have the latest package information)
- cabal sandbox init (create a local sandbox for all deps, instead of using a global)
- cabal install --only-dependencies

## Running the game

- cabal run
