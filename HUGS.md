# Hugs98 TidalCycles

test with:

```
hugs -98 +o -E"nano +%d %s" -P:hugs:src:tidal-link/src/hs Sound.Tidal.Context
```

(after building/patching `Link.so` as below)

OSC output is broken for now,
and there are no OverloadedStrings
but you can print patterns in the REPL:

```
> s (fromString "[bd sn, hh*4]")
```

check it still works with ghc:

```
ghci \
  -package containers \
  -package hosc \
  -package mtl \
  -package network \
  -package parsec \
  -package random \
  -package stm \
  -XOverlappingInstances \
  -XUndecidableInstances \
  -O2 \
  -ighc:src:tidal-link/src/hs \
  Sound.Tidal.Context
```

## tidal-link FFI

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

## Changes

Control.Applicative imported more places

import Prelude hiding not needed

Data.Monoid imported where necessary

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
