## [unreleased]

### ⛰️  Features

- *(base)* Add set-record-type-printer! - ([9e9c749](https://codeberg.org/playXE/capy/commit/9e9c749a19551a39c7b9985e137ddd2cf231a19e))
- *(capy)* Add `do*` macro - ([6cc2b84](https://codeberg.org/playXE/capy/commit/6cc2b84bdeed0e77c2b58c0becaf7094ca3cde75))
- *(ci)* Add git-cliff to nightly workflow - ([4f344ae](https://codeberg.org/playXE/capy/commit/4f344ae187c8d9b233d9b90a9b0fc1d9f33df4b6))
- *(compile-psyntax)* Squeeze tree-il properly - ([5385193](https://codeberg.org/playXE/capy/commit/5385193b50167240168e474b79b0446bf036bfaa))
- *(compiler)* Move all primitive expanders from Rust to Scheme - ([8b4bc7c](https://codeberg.org/playXE/capy/commit/8b4bc7c50061dbed16579d6e8f8d09d58f77a542))
- *(io)* Allow syscall:open to open RDWR files - ([044a33f](https://codeberg.org/playXE/capy/commit/044a33f4a150f730b9ce88cc250757ad6a19c70f))
- *(makefile)* Pass -j to compile-all - ([10311aa](https://codeberg.org/playXE/capy/commit/10311aa0cda65ed349863d4581429a5dd3143ec9))
- *(psyntax)* Add identifier-binding - ([a6b29a8](https://codeberg.org/playXE/capy/commit/a6b29a879ddfb016de953b12981b877c5df02217))
- *(tests)* Add source location to R6RS tests - ([f1a6d0b](https://codeberg.org/playXE/capy/commit/f1a6d0b0652593a518ffd9a852ae2fe0e4c3f879))
- Rewrite resolve-primitives in Scheme - ([a1fba8d](https://codeberg.org/playXE/capy/commit/a1fba8d0fa8cc7b30dc1f057591d7217857799d0))

### 🐛 Bug Fixes

- *(boot)* Add inf?, $set-attachments! and $winders wrapper functions - ([60801f4](https://codeberg.org/playXE/capy/commit/60801f4592bd3adc0e28c6af2dec27392c847ab5))
- *(ci)* Fix git-cliff version - ([6c3c624](https://codeberg.org/playXE/capy/commit/6c3c62492497ebedb46d0fdfb1dc2eb5c68b6976))
- *(ci)* Label names fix - ([cf5263f](https://codeberg.org/playXE/capy/commit/cf5263f1207716478f8477f6f1978ba098569844))
- *(compile-psyntax)* Compile psyntax in (capy) module - ([c3c541c](https://codeberg.org/playXE/capy/commit/c3c541ca3b3e25fd2876aaf913965023fbafc1a2))
- *(io)* Make input/output ports work - ([96ed753](https://codeberg.org/playXE/capy/commit/96ed753f4f578fe0f9929aaabb12cc94943e9657))
- Allow R6RS let-syntax with empty body - ([7a7c163](https://codeberg.org/playXE/capy/commit/7a7c163412523d94a65de0f8ed071d720cda7bc9))
- Make empty string comparison work - ([0bde187](https://codeberg.org/playXE/capy/commit/0bde187375f5b1ab493c4a8706e5195ca9c048c2))
- Write flonums with dot included - ([d9a5ef0](https://codeberg.org/playXE/capy/commit/d9a5ef074bd80adffdf0c267b554b7c8e642b4af))

### 🚜 Refactor

- *(capy)* Formatting - ([f90e0a7](https://codeberg.org/playXE/capy/commit/f90e0a78664ceb47c744402e71dd8deaf7ebd4f7))
- *(ci)* Mark nightly release as pre-release - ([f1351b9](https://codeberg.org/playXE/capy/commit/f1351b98207b9ebd61a5f62d05e8dd071ca620ab))
- Split tree-il module into multiple files - ([a8d087c](https://codeberg.org/playXE/capy/commit/a8d087c0354d547c53434d06de640f3a5f6fdeb8))

### ⚙️ Miscellaneous Tasks

- Update CHANGELOG.md [skip ci] - ([16d81bc](https://codeberg.org/playXE/capy/commit/16d81bcd2b9549a142ba6d8051b06a5112f06f94))


## [1.0.0] - 2026-01-03

### ⛰️  Features

- *(base)* Cond-expand - ([66e9e04](https://codeberg.org/playXE/capy/commit/66e9e04b237bf06b2aca3dc0c287642fddc9ecea))
- *(batteries)* Ropes - ([195175c](https://codeberg.org/playXE/capy/commit/195175cb18274f64eaba5a59d6bd4f4ced75cff2))
- *(boot)* Start work on psyntax - ([787318e](https://codeberg.org/playXE/capy/commit/787318ee774930f78670925577357049f8f4e113))
- *(cli)* Add `--append-load-path` (-A) - ([71a4192](https://codeberg.org/playXE/capy/commit/71a419229452cb9594f710fd86cd4fb900c771d2))
- *(compiler)* Letrectification - ([835ca33](https://codeberg.org/playXE/capy/commit/835ca33a61321ea69ecc714c82a7c30961517d96))
- *(contify)* Support for handler continuations - ([050f0d0](https://codeberg.org/playXE/capy/commit/050f0d050a876c1f62be0e5ea3684a2d6c93f25e))
- *(io)* Default UTF-8 encoding; port-fd - ([e49aaa7](https://codeberg.org/playXE/capy/commit/e49aaa7e29de5072bbf3e7511af8182b554466b1))
- *(print)* Add #<unspecified> printer - ([2858efb](https://codeberg.org/playXE/capy/commit/2858efb20215676b4211f5551d939e8e9657fa6d))
- *(runtime)* Add syntax transformer type - ([bbc2761](https://codeberg.org/playXE/capy/commit/bbc2761d8ba72d0ee21a3331cc25d40e283ed5a2))
- *(vm)* More debugging - ([cc6c346](https://codeberg.org/playXE/capy/commit/cc6c3462dbdec76f2bc908fcb94e0fdbe9c24ff5))
- Scheme lib - ([47eba5f](https://codeberg.org/playXE/capy/commit/47eba5f1556ac0383561f9cb8107c73680d69b44))
- Allow allocating nonmovable bytevectors - ([ef53103](https://codeberg.org/playXE/capy/commit/ef5310373e5268babe99de89498abaf4eeb3e373))
- Enable generational GCs and write-barriers - ([53115d9](https://codeberg.org/playXE/capy/commit/53115d9c75df5df9981fd6f199f54387459cd419))
- Getcwd - ([ecb2d3e](https://codeberg.org/playXE/capy/commit/ecb2d3eccbc2144730146bd432fb68281f7f3cb2))
- Program-arguments - ([5b6b3cc](https://codeberg.org/playXE/capy/commit/5b6b3ccce96199bd13932afc0b8bac397b7ecc57))
- Add portable build - ([d1263e1](https://codeberg.org/playXE/capy/commit/d1263e1d503137b65dcc5e758f55cad2afb8eaa3))
- Primref -> Cps - ([c29d251](https://codeberg.org/playXE/capy/commit/c29d2516a24ca59d3f335ad27462d3a9e2987ad8))
- Add CLI and read - ([90dd0ab](https://codeberg.org/playXE/capy/commit/90dd0ab7bc95e4ec60096ad2d8ad68227e20e537))
- Add primitive-load - ([df09377](https://codeberg.org/playXE/capy/commit/df09377141a02a24f647b39ff46f634e35e2cdaf))

### 🐛 Bug Fixes

- *(boot)* Make accessor and mutator for records safe - ([bc5fd4c](https://codeberg.org/playXE/capy/commit/bc5fd4cc66ebefb4c35ea63b5eda49a23218a6df))
- *(cps)* Make sequence compile properly when in tail position - ([c0f72ea](https://codeberg.org/playXE/capy/commit/c0f72ea5447cc07574cb8e7eb192d0cd3c3a1080))
- *(cps)* Enable DCE of functions - ([d33b54f](https://codeberg.org/playXE/capy/commit/d33b54f5bc0021b6878c865eaf528e1d41795652))
- *(expander)* Produce properly ordered fixes - ([2dd528e](https://codeberg.org/playXE/capy/commit/2dd528ee510e4fc811f9ac6548c02910a84151f0))
- *(foreign)* Proper align calculation - ([bccc1c4](https://codeberg.org/playXE/capy/commit/bccc1c4539a814f3c1f6b7d7b77201a7754f429b))
- *(frontend)* Do not annotate simple values - ([534655b](https://codeberg.org/playXE/capy/commit/534655bd76387e105ba546ca3dc109d47686c79f))
- *(load)* Better lookup of files - ([20f1835](https://codeberg.org/playXE/capy/commit/20f183526cb2eb0c788a1b7e78801ddad49eac55))
- *(numbers)* Add header to Complex and Rational objects - ([f726183](https://codeberg.org/playXE/capy/commit/f72618393b03e00eb7fa82fca7c10ad809fd8675))
- *(psyntax)* Re-wrap syntax object to preserve module information - ([930b277](https://codeberg.org/playXE/capy/commit/930b27724d558b1f68ee3073b28dd7e66956877c))
- *(psyntax)* Better source information in datum->syntax - ([b50945e](https://codeberg.org/playXE/capy/commit/b50945eef44b9a4cead326591e0692ba64b48935))
- *(reader)* Wrap objects into syntax only once - ([fad0ced](https://codeberg.org/playXE/capy/commit/fad0ced5299bfc3aeb6ffc8b59428434e3e44c06))
- *(threading)* Wait for thread to be in %thread-join-data - ([33b7841](https://codeberg.org/playXE/capy/commit/33b7841ab27e6faca8fd378121115efd56ab765b))
- Once candidate found break out of search loop properly - ([68c5f98](https://codeberg.org/playXE/capy/commit/68c5f98a9e3a917df447b1ae1d0192eec6a4c540))
- Disable reordering for now - ([cec06ff](https://codeberg.org/playXE/capy/commit/cec06ff76a8d624cfb08f9395fa88426f24e7bcd))
- Add version to libraries - ([a78ff2a](https://codeberg.org/playXE/capy/commit/a78ff2a5ce89b282544bac34b7c35232d0525107))
- Fix extension list - ([7bb7fa4](https://codeberg.org/playXE/capy/commit/7bb7fa469bd2090dfd1f06698fad4d7a0981ca1f))
- Fix expansion of some primitives - ([da87252](https://codeberg.org/playXE/capy/commit/da8725250fa2c6444db85751ca3ab1c912948a13))

### 🚜 Refactor

- *(runtime)* Make modules declarative by default - ([a2899b1](https://codeberg.org/playXE/capy/commit/a2899b18c83c2bb073aa3572d06d8f52fd874732))

### Keywords

- Zero-cost keywords and `define` with keywrods support - ([95961f6](https://codeberg.org/playXE/capy/commit/95961f6c8317e6fbb18fc6b25b229dddfe738197))

### Psyntax

- Recursively wrap expressions - ([2cb780b](https://codeberg.org/playXE/capy/commit/2cb780ba3645efd09b48c1ac2d57faa970594001))
- Simplify ribcage-has-var? - ([0914539](https://codeberg.org/playXE/capy/commit/0914539fdc7805ef74459480a845c1cdb47a96dd))

### Stdlib

- Add define* and lambda* (keywrod arguments) - ([4a8a6bb](https://codeberg.org/playXE/capy/commit/4a8a6bbbdf3617b75bde65c4252b0b5cb20a9a40))

## New Contributors

* @ made their first contribution in [#15](https://codeberg.org/playXE/capy/pull/15)
* @playXE made their first contribution
* @syohex made their first contribution

<!-- generated by git-cliff -->
