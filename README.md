# Ankka

## Setting up the game

### Ubuntu
- Install necessary dependencies: `sudo apt-get install -y libglu1-mesa-dev libxrandr-dev libxcursor-dev libxinerama-dev libxi-dev mesa-common-dev libx11-dev libgl1-mesa-dev libftgl-dev cpphs`.
- Install Cabal + GHC `sudo apt-get install -y cabal-install-1.18 ghc-7.8.4`. Recommended GHC version is 7.8.x. Recommended version of Cabal is 1.18.0.x (otherwise you may not be able to get ghc-mod to work). You may also install these as part of `haskell-platform` (which includes many other things as well -- but it's harder to specify particular versions this way). You may use newer versions of Cabal and GHC, but we have had issues with ghc-mod.
- `cabal update`
- `cabal sandbox init`
- `cabal install --only-dependencies`
- `cabal configure`
 
### Windows
- Install the Haskell platform from www.haskell.org
- Download [`ftgl-2.1.3-vc11-64`](http://www.opencascade.org/getocc/download/3rdparty/) (or 32bit) and place the binary DLL in SysWOW64
- Do the same for [`freetype-2.5.5-vc12-64`](http://www.opencascade.org/getocc/download/3rdparty/) (or 32bit)
- `cabal update`
- `cabal sandbox init`
- Install FTGL: `cabal install ftgl --extra-include-dirs="C:\ftgl-2.1.3-vc11-64\include" --extra-lib-dirs="C:\ftgl-2.1.3-vc11-64\lib" --reinstall --force-reinstalls`
- `cabal install --only-dependencies`
- `cabal configure`

## Running the game

- `cabal run`
