# Hugs98 TidalCycles

test with:

```
hugs -98 -E"nano +%d %s" -P:hugs:src:tidal-link/src/hs Sound.Tidal.Context
```

(after building/patching `Link.so` as below)

OSC output is broken for now,
and there are no OverloadedStrings
but you can print patterns in the REPL:

```
> s (fromString "[bd sn, hh*4]")
```

## tidal-link FFI

there are some patches in `tidal-link/src/hs/Sound/Tidal`
for compiling `tidal-link` with `ffihugs`

- you need a ghc installation for hsc2hs...
- `hsc2hs -o tidal-link/src/hs/Sound/Tidal/Link.hs -I tidal-link/link/extensions/abl_link/include/ tidal-link/src/hs/Sound/Tidal/Link.hsc`
- `cd tidal-link/src/hs`
- patch `Sound/Tidal/Link.hs` with the hsc2hs patch file
- `ffihugs Sound/Tidal/Link.hs -I../../link/extensions/abl_link/include`
  (this will fail and print the command that failed)
- patch the output `Link.c` with the ffihugs patch file
- run the failing build command that was printed again
- you should get a `Link.so` that you can load into hugs

## Changes

Control.Applicative imported more places

import Prelude hiding not needed

Data.Monoid imported where necessary

ValueMap is a newtype instead of a type to avoid overlapping instances

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
