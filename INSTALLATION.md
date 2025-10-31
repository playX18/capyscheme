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
$ just prefix=/usr/local/share install-portable 

```

After everything is installed you should add installed directory into PATH and run `capy` binary for the
first time to build boot library. If you wish to use native extensions also add `extensions/` directory under prefix
into your LD_LIBRARY_PATH/DYLD_FALLBACK_LIBRARY_PATH.

First boot should take around 30-40 seconds on decent systems, after that please restart the binary if you're
entered interactive mode. 

### NOTE

First boot might consume huge amounts of memory (8GiB+). This is fine and should not happen again. The only
reason this happens is that GC is not able to run during CPS compiler optimizations and it allocates a lot of 
new IR nodes during that which consume memory *very quickly&*. MMTK also does not help here as it does not
unmap free pages so you have to either rely on your OS unmapping them or just have a lot of RAM. Build was tested
on Macbook Air M4 with 16GiB of RAM and i5-13400f wit 64GiB of RAM.