# ankka

## Setting up the game

- git clone
- Install the Haskell platform along with the cabal package manager
- cabal update (just to make sure you have the latest package information)
- cabal sandbox init (create a local sandbox for all deps, instead of using a global)
- cabal install --only-dependencies

## Running the game

- cabal run
