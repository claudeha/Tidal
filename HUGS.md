# Hugs98 TidalCycles

you need Hugs2019 plus experimental OverloadedStrings from:
<https://github.com/claudeha/Hugs/tree/OverloadedStrings>
(which was forked from <https://github.com/cjacker/Hugs>)

## deepseq

Hugs2019 fork does not (yet) bundle the deepseq package.
replace `${HOME}/opt/hugs` with your Hugs installation prefix:

```
wget https://hackage.haskell.org/package/deepseq-1.1.0.2/deepseq-1.1.0.2.tar.gz
tar xaf deepseq-1.1.0.2.tar.gz
cd deepseq-1.1.0.2
runhugs -98 Setup.hs configure --prefix=${HOME}/opt/hugs
runhugs -98 Setup.hs build
runhugs -98 Setup.hs install
```

## install

Replace `${HOME}/opt/hugs` with your installation prefix:

```
runhugs -98 Setup.hs configure --prefix=${HOME}/opt/hugs
runhugs -98 Setup.hs build
runhugs -98 Setup.hs install
```

## run

After installing, assuming your installed Hugs is in your shell path:

```
hugs -98 +o -E"nano +d %s" Sound.Tidal.Context
```

Quit with `:q`.

Use `:f echoWith` (for example) to jump to source code of definitions
(`Ctrl x` to exit the nano text editor).

## hacking

To run without installing from the Tidal source directory:

```
hugs -98 +o -E"nano +%d %s" -Phugs:src: Sound.Tidal.Context
```

Then use `:e` when an error occurs to edit the offending line.

Hugs may get confused with already-installed tidal,
you can delete or move `${HOME}/opt/hugs/lib/hugs/packages/tidal`
out of the way (replacing `${HOME}/opt/hugs` with your Hugs
installation directory.

## caveats

all realtime stuff is gone:

- no stateful REPL for triggering patterns
- no OSC to make other software make sounds
- no Ableton Link to keep in sync with peers

but you can print and draw patterns in the REPL:

```
> s "[bd sn, hh*4]"
> drawLine "x ~ o ~"
```

## ghc

check it still works with ghc:

```
ghci \
  -package containers \
  -package deepseq \
  -package mtl \
  -package parsec \
  -package random \
  -XOverloadedStrings \
  -XOverlappingInstances \
  -XUndecidableInstances \
  -ighc:src \
  Sound.Tidal.Context
```

## benchmarks

hugs is quick to start up (but interprets code slowly):

```
$ ( echo :main ; echo :q ) | time hugs ... Test.hs
1.10user 0.10system 0:01.21elapsed 99%CPU (0avgtext+0avgdata 14256maxresident)k
0inputs+0outputs (0major+1913minor)pagefaults 0swaps
```

ghci is slow to start up (but interprets code quickly):

```
$ ( echo :main ; echo :q ) | time ghci ... Test.hs
3.31user 0.18system 0:03.49elapsed 100%CPU (0avgtext+0avgdata 398452maxresident)k
0inputs+8outputs (0major+66800minor)pagefaults 0swaps
```

ghc optimized code is fastest,
if you're prepared to wait even longer for compilation:

```
$ time ghc ... -O2 -fforce-recomp Test.hs
35.38user 0.70system 0:36.06elapsed 100%CPU (0avgtext+0avgdata 588324maxresident)k
0inputs+29880outputs (70major+213127minor)pagefaults 0swaps
$ time ./Test
0.00user 0.00system 0:00.00elapsed 85%CPU (0avgtext+0avgdata 7616maxresident)k
0inputs+0outputs (0major+680minor)pagefaults 0swaps
```

a hybrid approach with ghc may be best:
only first start is slow, subsequent starts fast

```
$ ( echo :main ; echo :q ) | time ghci ... -O2 -fobject-code -fforce-recomp Test.hs
35.39user 0.65system 0:36.04elapsed 100%CPU (0avgtext+0avgdata 592104maxresident)k
0inputs+15264outputs (0major+175064minor)pagefaults 0swaps
$ rm Test.o Test.hi
$ ( echo :main ; echo :q ) | time ghci ... -O2 -fobject-code Test.hs
0.52user 0.14system 0:00.67elapsed 100%CPU (0avgtext+0avgdata 226064maxresident)k
0inputs+112outputs (1major+43511minor)pagefaults 0swaps
```

testing just startup time (remove `echo :main`) seems to show that
interpreting the code in Test.hs for the first time
adds about 0.2 seconds for all implementations

## Changes

non-realtime only

- dropped Sound.Tidal.Stream
- dropped Sound.Tidal.Transition
- no OSC (so no sound)
- no Link (so no sync with peers)

compatibility shims in hugs/ vs ghc/ directories

import Prelude hiding replaced with custom MyPrelude
because Hugs breaks when non-exported things are hidden

Control.Applicative imported more places

Data.Monoid imported where necessary

Data.Semigroup stub for Hugs

LambdaCase : replaced by use of lambda and case of

BangPatterns : replaced by use of seq

Typeable, Generic : gone, NFData instances for deepseq implemented by hand

DeriveFunctor, GeneralizedNewtypeDeriving, ... : implemented instances by hand

coerce : replaced with (fmap) realToFrac

Sound.Tidal.Show merged into Sound.Tidal.Pattern because Num and IsString need Show in Hugs

misc utility functions added:

- bool, intercalate, sortOn, (>=>)

Parsec

- adapted to older version bundled with Hugs
- error messages may be poor
- chords are commented out for now, as Hugs doesn't have GADTs
- the rest of TPat is converted to a regular ADT

Colour

- commented out for now
- need to see if dependency package can be used with Hugs

Text

- src/Sound/Tidal/UI.hs has a new splitComma thing that doesn't depend on the text package
- tested with QuickCheck, seems to be identical to the original

CPP

- src/Sound/Tidal/Utils.hs conditional compilation removed (we have left Glasgow)

Paths_tidal

- custom hardcoded thing for Hugs
