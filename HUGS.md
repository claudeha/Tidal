# Hugs98 TidalCycles

test with hugs98 as found in Debian:

```
hugs -98 +o -E"nano +%d %s" -P:hugs98:src:tidal-link/src/hs Sound.Tidal.Context
```

or for the hugs2019 updated repository
<https://github.com/cjacker/Hugs>:

```
hugs -98 +o -E"nano +%d %s" -P:hugs2019:hugs98:src:tidal-link/src/hs Sound.Tidal.Context
```

(after building/patching `Link.so` as below)

OSC output is broken for now,
and there are no OverloadedStrings
but you can print and draw patterns in the REPL:

```
> s (fromString "[bd sn, hh*4]")
> drawLine (fromString "x ~ o ~")
```

check it still works with ghc:

```
ghci \
  -package containers \
  -package deepseq \
  -package hosc \
  -package mtl \
  -package network \
  -package parsec \
  -package random \
  -package stm \
  -XOverlappingInstances \
  -XUndecidableInstances \
  -ighc:src:tidal-link/src/hs \
  tidal-link/link/build/libabl_link.so \
  Sound.Tidal.Context
```

## deepseq

use correct `--prefix` for your Hugs installation:

```
wget https://hackage.haskell.org/package/deepseq-1.1.0.2/deepseq-1.1.0.2.tar.gz
tar xaf deepseq-1.1.0.2.tar.gz
cd deepseq-1.1.0.2
runhugs Setup.hs configure --prefix=${HOME}/Hugs
runhugs Setup.hs build
runhugs Setup.hs install
```

## tidal-link FFI

### for hugs

there are some patches in `tidal-link/src/hs/Sound/Tidal`
for compiling `tidal-link` with `ffihugs`

`hsc2hs-hugs` is broken on Debian (missing alignment function):
there is also a hardcoded default path that doesn't exist.
hack to fix it:

```
apt source hugs98
patch -p1 < tidal-link/src/hs/Sound/Tidal/hsc2hs-alignment-fixes-for-hugs.patch
sudo mkdir /usr/share/hsc2hs-0.67
sudo cp hugs98-98.200609.21/hsc2hs/template-hsc.h /usr/share/hsc2hs-0.67
```

alternatively to sudo: use the `-t` argument for `hsc2hs-hugs`

then build the ffi module for hugs

```
hsc2hs-hugs -o tidal-link/src/hs/Sound/Tidal/Link.hs -I tidal-link/link/extensions/abl_link/include/ tidal-link/src/hs/Sound/Tidal/Link.hsc
ffihugs -P:hugs tidal-link/src/hs/Sound/Tidal/Link.hs -Itidal-link/link/extensions/abl_link/include
# the previous command fails and prints a build command, but continue:
patch -p1 -F3 < tidal-link/src/hs/Sound/Tidal/ffihugs-ableton-link-fixes.patch
# rerun the failed build command, in my case it was:
gcc -Wall -fPIC -std=gnu89   -shared -fPIC -D__HUGS__ "-I/usr/lib/hugs/include" -o "tidal-link/src/hs/Sound/Tidal/Link.so" "tidal-link/src/hs/Sound/Tidal/Link.c" -Itidal-link/link/extensions/abl_link/include
```

you should now have a `Link.hs` and `Link.so` that you can use in Hugs

### for ghci

you need a shared `abl_link` library for `ghci`:

```
mkdir tidal-link/link/build
cd tidal-link/link/build
cmake -DCMAKE_CXX_FLAGS=-fPIC -DLINK_TESTS=OFF ..
make
g++ -shared -Wl,--whole-archive libabl_link.a -Wl,--no-whole-archive -o libabl_link.so
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

compatibility shims in hugs/ vs ghc/ directories

import Prelude hiding replaced with custom MyPrelude
because Hugs breaks when non-exported things are hidden

Control.Applicative imported more places

Data.Monoid imported where necessary

Data.Semigroup stub for Hugs

LambdaCase : replaced by use of lambda and case of

BangPatterns : replaced by use of seq

Typeable, Generic, NFData (deepseq) : gone

DeriveFunctor, GeneralizedNewtypeDeriving, ... : implemented instances by hand

OverloadedStrings is not present : must use parseBP_E (or fromString) explicitly

Exception : use old non-extensible version

coerce : replaced with (fmap) realToFrac

Sound.Tidal.Show merged into Sound.Tidal.Pattern because Num needs Show in Hugs

misc utility functions added:

- bool, intercalate, sortOn, (>=>)

Parsec

- adapted to old version bundled with Hugs on Debian
- error messages may be poor
- chords are commented out for now, as Hugs doesn't have GADTs
- the rest of TPat is converted to a regular ADT

Colour

- commented out for now
- need to see if dependency package can be used with Hugs

OSC

- Sound/Tidal/Stream/Target.hs has various hacks
- it is broken/non-functional

Text

- src/Sound/Tidal/UI.hs has a new splitComma thing that doesn't depend on the text package
- tested with QuickCheck, seems to be identical to the original

CPP

- src/Sound/Tidal/Utils.hs conditional compilation removed (we have left Glasgow)

Paths_tidal

- custom hardcoded thing for Hugs

STM

- Hugs doesn't have retry, patched to do retries manually with busy loop and yield...
