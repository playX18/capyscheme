# Installation

To install & run CapyScheme you need additonal dependencies:
- Just: for running Justfile recipe
- lld: for linking resulting Scheme dynamic libraries. Right now it is hardcoded, but in the future
it should be possible to change it

After they are installed simply run:
```sh
$ just install-portable
```

Or if you want different installation prefix:
```sh
$ just install-prefix=/usr/local/share install-portable 

```

After everything is installed you should add installed directory into PATH.
If you wish to use native extensions also add `extensions/` directory under prefix
into your LD_LIBRARY_PATH/DYLD_FALLBACK_LIBRARY_PATH.

## GC limitations

Once the `capy` binary and heap image are built the GC algorithm is locked in. For example
if you built with `MMTK_PLAN=GenImmix` env variable set you can only change GC plans to GenCopy 
or StickyImmix when running this binary. 

If you want to run with concurrent Immix simply perform build with `MMTK_PLAN=ConcurrentImmix` env variable
set.