## [unreleased]

### ⛰️  Features

- *(api)* Expose UTF-8 value helpers - ([a3ffbc5](https://codeberg.org/playXE/capy/commit/a3ffbc5d009768703f80c0dedc63f816c3f84cff))
- *(build)* Install and package Capy LSP - ([00dba66](https://codeberg.org/playXE/capy/commit/00dba66403e286b42cfae16b6171ad564a265623))
- *(capy)* Infer switches - ([d18b552](https://codeberg.org/playXE/capy/commit/d18b55228851263d81329b01357b14f849afb270))
- *(capy)* Linear CPS - ([7d6e43b](https://codeberg.org/playXE/capy/commit/7d6e43ba8f58aaed568c5b04645ef6d2b88231df))
- *(capy/compiler)* Greedy slotalloc - ([c2c7025](https://codeberg.org/playXE/capy/commit/c2c7025c141c81b6c5845755e817ece52f507fa6))
- *(capy/cps)* Hoist constants - ([54473ba](https://codeberg.org/playXE/capy/commit/54473ba6569b43ddd3811032ede8095cd7447907))
- *(capy/gc)* Add benchmarks - ([87e06e9](https://codeberg.org/playXE/capy/commit/87e06e9f14393658a53f51692b5ebf658ac57bb6))
- *(capy/gc)* Add CLI options for GC - ([c1f6323](https://codeberg.org/playXE/capy/commit/c1f6323469495d1d8b3ce80fe0894dcaca46e01d))
- *(capy/gc)* Custom trigger policy - ([084f3ed](https://codeberg.org/playXE/capy/commit/084f3ed0c07a6549e0c857c0fc5174d05959369d))
- *(compiler)* Optimize rest arguments to direct access - ([76e7a05](https://codeberg.org/playXE/capy/commit/76e7a056cf0b938fd37a38e2bdd3e24503cb7f81))
- *(control)* Implement call-in-continuation and add tests for its behavior - ([7fce732](https://codeberg.org/playXE/capy/commit/7fce732bfd1228654c1899cf1c8c0424be0882f8))
- *(library)* Auto-import capy prelims - ([3116a3f](https://codeberg.org/playXE/capy/commit/3116a3f471f7f1bf229a60216029ef0b2b09bc38))
- *(lsp)* Build workspace index on worker side - ([88fd7fc](https://codeberg.org/playXE/capy/commit/88fd7fc4f760ab5a9efc78fc0851e15b999336ee))
- *(lsp)* CallGraph - ([a7d2ebe](https://codeberg.org/playXE/capy/commit/a7d2ebec1d75a3fe8aa3051aa2fb2dd1e7fb66e5))
- *(lsp)* Add refactored Rust language server - ([12382c8](https://codeberg.org/playXE/capy/commit/12382c8d29247d2409d7ce9dfd9a097a27fdfed5))
- *(lsp)* Add Scheme worker analysis - ([689d7aa](https://codeberg.org/playXE/capy/commit/689d7aa581242527135bd060d1343be4f4c59b3b))
- *(lsp)* Add isolated VM worker launcher - ([36d267b](https://codeberg.org/playXE/capy/commit/36d267b735921bae7da68db6644565f45427b7c8))
- *(psyntax)* Add default core #%app - ([0c8d46c](https://codeberg.org/playXE/capy/commit/0c8d46c48f3fdf275acdc2a0dad6a034f6416dd7))
- *(psyntax)* Route calls through implicit #%app - ([4192658](https://codeberg.org/playXE/capy/commit/419265893227c2a8f56a7ecebdce8fee1fcee9a1))
- *(psyntax)* Define-property - ([1dd6932](https://codeberg.org/playXE/capy/commit/1dd69323e87e691da282e613fb03ed5b8c0cd50c))
- *(srfi)* Add custom comparator to SRFI-64 - ([1ea56e3](https://codeberg.org/playXE/capy/commit/1ea56e3278a80df9eb397fb3f02b50d0ea1f6a36))
- *(srfi)* SRFI-213 full implementation - ([db8ebc7](https://codeberg.org/playXE/capy/commit/db8ebc7df8a292b6d1a9a30c692a265cb36cb9d8))
- *(tests)* Use srfi-64 test runner for racket/r6rs test suite - ([0ca56c9](https://codeberg.org/playXE/capy/commit/0ca56c9ed305d6528c9c56be31d37a483dd74fba))
- *(vscode)* Add Capy Scheme LSP extension - ([180ec16](https://codeberg.org/playXE/capy/commit/180ec160da7aa46f8b6c981d6a59961ef96e036f))
- Use tree il as source of truth for lsp - ([842dc7a](https://codeberg.org/playXE/capy/commit/842dc7a1a4c6e343952aa8ebb83196eb70537780))

### 🐛 Bug Fixes

- *(capy/gc)* Mutator always used 8 bytes alignment, use max(alignment, 8) - ([577c283](https://codeberg.org/playXE/capy/commit/577c283c91b61dcb7b7e930437838aa359542f7d))
- *(cps)* Lower to wrong arity condition instead of panicking in compiler - ([4ecdeca](https://codeberg.org/playXE/capy/commit/4ecdecaa0f4c1a929fe339b4aa8ea12d756d7c6e))
- *(lib)* Fix #%app imports - ([23d5085](https://codeberg.org/playXE/capy/commit/23d50859053535873404050e35ccee128280173d))
- *(lib/capy)* Better threading - ([712093f](https://codeberg.org/playXE/capy/commit/712093f4a25348acec0393f9325d51d6814b046b))
- *(lib/core)* Mutex protect data - ([e551a91](https://codeberg.org/playXE/capy/commit/e551a911a1704e6744ff456c2491a27dfa2d1742))
- *(psyntax)* Stabilize implicit #%app bootstrap - ([5df6bc0](https://codeberg.org/playXE/capy/commit/5df6bc03e6114e320377034c9bf6496c2f03d325))
- Dead code - ([2579e97](https://codeberg.org/playXE/capy/commit/2579e972148f3b19c5aec6575dd2b6104e4fa81f))
- Remove redundant/outdated tests - ([47c0fd4](https://codeberg.org/playXE/capy/commit/47c0fd495b645fe08a0d1476eb860ad0d982cffc))
- Remove outdated/redundant tests - ([0bac374](https://codeberg.org/playXE/capy/commit/0bac374af19e7daf60fab38a3538fcb01b04bc0e))
- Remove cached repo - ([cae021d](https://codeberg.org/playXE/capy/commit/cae021dbeacba2e891cc9d38cd0b1e4033e0044c))

### 🚜 Refactor

- *(capy)* Better CPS memory usage - ([3f3b0ff](https://codeberg.org/playXE/capy/commit/3f3b0ff99fafa622814e7df026d44f9a0ac8c8ee))
- *(capy)* Infer switches even if block has more than 2 opcodes - ([14040be](https://codeberg.org/playXE/capy/commit/14040be4c9394ffff7242763a8380080196f4e34))
- *(capy/gc)* Split heuristics into files; better adaptive heuristic - ([ffdfdac](https://codeberg.org/playXE/capy/commit/ffdfdacf19be683c978d0ab50b8ddaa365e51c0d))
- *(compiler)* Use ValueId in linear CPS - ([e6b26bd](https://codeberg.org/playXE/capy/commit/e6b26bd224ad526b505e4ddb76ecd41c9fa6d0ed))
- *(lsp)* Restart on file changes instead of fighting with autoload - ([67e3cb8](https://codeberg.org/playXE/capy/commit/67e3cb8ae1302353deff8f567a6b01c6a124c5b8))
- *(lsp)* Invalidate files differently - ([1e1b74c](https://codeberg.org/playXE/capy/commit/1e1b74c0cfd9311fdf0d22532371821e546ac08d))
- *(runtime)* Better source handling - ([83a33e2](https://codeberg.org/playXE/capy/commit/83a33e2a32f53303269c26fc022291a2b975d3f6))

### 📚 Documentation

- Design cps block macro - ([95ac1d4](https://codeberg.org/playXE/capy/commit/95ac1d40fdc36b4a014b6d0d517184935aa12919))

### 🧪 Testing

- Cover implicit #%app expansion - ([b6f2633](https://codeberg.org/playXE/capy/commit/b6f263318e24bedb7556b847e6e0dcde8422b13d))

### ⚙️ Miscellaneous Tasks

- Ignore worktree directory - ([e9d3214](https://codeberg.org/playXE/capy/commit/e9d3214f42b9da9dbb23c18536d37c50febbef26))

### ◀️ Revert

- *(capy/cps)* Optimization caused bugs in bootstrap - ([19e16cf](https://codeberg.org/playXE/capy/commit/19e16cf0a3e320bdbfac8904f8131f977daf08de))

### Build

- Regenerate psyntax for implicit #%app - ([72523ed](https://codeberg.org/playXE/capy/commit/72523edc300ce198e81f0ad32420398622fc4c3d))

### Refator

- *(capy/gc)* Measure GC start/end using GCTriggerPolicy - ([90f48b5](https://codeberg.org/playXE/capy/commit/90f48b5fd7d7ebbd12464a8f891b37d672b33113))


## [1.3.0](https://codeberg.org/playXE/capy/compare/v1.2.0..v1.3.0) - 2026-04-27

### ⛰️  Features

- *(boot)* Optimize reader - ([9e38523](https://codeberg.org/playXE/capy/commit/9e385230243717fdaeacac7020fa3ed544c43310))
- *(runtime)* Start work on interpreter - ([6cc1efb](https://codeberg.org/playXE/capy/commit/6cc1efb63a2acd74b2aee530fb69e762dc7d2cf4))
- *(tree-il)* Denoise tree-il->scheme; recompile psyntax - ([6079c8e](https://codeberg.org/playXE/capy/commit/6079c8e94facf63bf593643b117787c989f10830))
- Graphical CPS - ([2f7a1fb](https://codeberg.org/playXE/capy/commit/2f7a1fb3135d10a65c4d25377c8308e6f53014e6))

### 🐛 Bug Fixes

- *(ci)* Proper run of changelog - ([7ce7d69](https://codeberg.org/playXE/capy/commit/7ce7d69cc7c929cdbc7dd76b8a7f69068bbdb100))
- *(reader)* Less strict number parsing; fix cond match on string->number - ([c1f1bc4](https://codeberg.org/playXE/capy/commit/c1f1bc495181f59291fb3a1ace2784502f02f9e6))

### 🚜 Refactor

- *(cps)* Minor performance improvements - ([701f11f](https://codeberg.org/playXE/capy/commit/701f11fe78082cc72c88eab569e98509d2b97fc7))
- *(cps)* Improve inlining performance slightly - ([1071731](https://codeberg.org/playXE/capy/commit/1071731e1a1d640ca4649591a54a5d8d288778fb))
- *(load)* Split load into multiple files - ([74669e4](https://codeberg.org/playXE/capy/commit/74669e4f2889c252dd28e860aee16d9ffe6ba447))
- *(runtime)* Use dynamic side metadata addressclear - ([74a1719](https://codeberg.org/playXE/capy/commit/74a1719848e8b0726a558b93f611b5e1186a9006))
- *(runtime)* Switch to smaller headers - ([cd27f3d](https://codeberg.org/playXE/capy/commit/cd27f3d8f196e58edd8b2640ebfdd7f126b104a9))
- *(runtime)* Split load.rs into multiple files - ([a1b0848](https://codeberg.org/playXE/capy/commit/a1b08487de7fdc7cdc64a4cac8d587311d960576))
- Cleanup Rust code - ([d24f5a9](https://codeberg.org/playXE/capy/commit/d24f5a9ed8feea9e1574aa61d1449c7c48c1290b))
- Split number.rs into number/ module with bigint submodule - ([0dcb91a](https://codeberg.org/playXE/capy/commit/0dcb91a2a9f5376276a20b34cadf5f81b6713123))

### ⚙️ Miscellaneous Tasks

- Update dependencies - ([01cf78d](https://codeberg.org/playXE/capy/commit/01cf78d184c1ed95ffe402199817ae271ca7c509))
- Remove dead code - ([2c2375d](https://codeberg.org/playXE/capy/commit/2c2375df0bfe18754fcaf3e788716bcd9b2c915b))
- Update to latest nightly - ([6699ac6](https://codeberg.org/playXE/capy/commit/6699ac6de4dc9f66fd3033c0adca87a968c23eb6))
- Continued removal - ([e348c65](https://codeberg.org/playXE/capy/commit/e348c65b115bf6e48c4ce92c8e7b2a6620eafeae))
- Remove CPSSSA and JIT; redesign - ([e2d2573](https://codeberg.org/playXE/capy/commit/e2d25733d4e1d634cee8204dcce4a401c07c9885))


## [1.2.0](https://codeberg.org/playXE/capy/compare/v1.1.0..v1.2.0) - 2026-02-26

### 🐛 Bug Fixes

- *(makefile)* Trim `v` prefix from VERSION in dist-deb recipe - ([7491149](https://codeberg.org/playXE/capy/commit/7491149b2d2e786fa48eb5e76dd113651d37ac40))
- Fix up nightly job - ([017e362](https://codeberg.org/playXE/capy/commit/017e3626e25cd0abd8dc4ed8edf7e7e328e6fa91))


## [1.1.0](https://codeberg.org/playXE/capy/compare/1.1.0..v1.1.0) - 2026-02-25

### ⛰️  Features

- *(capy)* Add intrusive lists (capy intrusive dlist) and (capy intrusive slist) - ([44a7ffd](https://codeberg.org/playXE/capy/commit/44a7ffd22f87510b091531acd5a645416083c0ce))
- *(capy)* Add (capy future) and (capy generator) modules - ([60110a2](https://codeberg.org/playXE/capy/commit/60110a21329f7453f972514236810643f115b691))
- *(capy)* Add binary heap in (capy binaryheap) - ([4844858](https://codeberg.org/playXE/capy/commit/4844858d39a7ae29ec1ee0f1eed9a55030742026))
- *(capy)* Add deque and channel libraries - ([b199e73](https://codeberg.org/playXE/capy/commit/b199e732c7ec5a9d45545728d02ae262fef1f937))
- *(capyc)* Add --nobacktrace flag - ([8185020](https://codeberg.org/playXE/capy/commit/8185020315558d2f940781dbe4b56c1b35f3a8c9))
- *(core)* Propagate thread exceptions on join - ([8c0ce26](https://codeberg.org/playXE/capy/commit/8c0ce2610b26bee32c66b9b25e0fe0ff7f45049b))
- *(core)* Add tuple printer registry and safe struct predicate - ([f4a3d8a](https://codeberg.org/playXE/capy/commit/f4a3d8ac76c5b8ab0b34e1b37a7561a01f395a66))
- *(core/foreign)* Use keyword arguments - ([30f707d](https://codeberg.org/playXE/capy/commit/30f707d8c5130011e44a91ae3803bcd3f0111d14))
- *(ffi)* Add blocking calls support - ([bf23f49](https://codeberg.org/playXE/capy/commit/bf23f495b91866660d9a1896a644146b605e967f))
- *(gc)* Pinning roots - ([bf8a1af](https://codeberg.org/playXE/capy/commit/bf8a1af2b2c3e4c0b49107eb382824dd835bacaa))
- *(io)* Use microsecond timeouts for polling instead of milliseconds - ([eb50d27](https://codeberg.org/playXE/capy/commit/eb50d27ddb77aeaa364a5e55081ac8aeff86c5f1))
- *(psyntax)* Syntax-parameterize - ([52a9d80](https://codeberg.org/playXE/capy/commit/52a9d8078a541ba4ade8b1869d59cb871cc066f9))
- *(runtime)* Add Socket type - ([149a4d3](https://codeberg.org/playXE/capy/commit/149a4d35b08c931c5c35763f928aa1012a88a2db))
- *(runtime)* Use self-hosted primitive expansion after bootstrap - ([82c55a7](https://codeberg.org/playXE/capy/commit/82c55a76aa03b9f6e9211c79245380458e143f6f))
- *(srfi)* SRFI-214: Flexvectors - ([1a98b4e](https://codeberg.org/playXE/capy/commit/1a98b4e5ed496005f6d6a33ed3b81508cf5b5e0d))
- *(srfi)* SRFI-145: Assumptions - ([a79a542](https://codeberg.org/playXE/capy/commit/a79a5422edc5a56ad798036dfaa1d8f7367200de))
- *(srfi-257)* Simple extendable pattern matcher with backtracking - ([2c435b4](https://codeberg.org/playXE/capy/commit/2c435b477755665647079a14da566dde96448da1))
- *(ssa)* Add (unspecified) lowering - ([2b9fba2](https://codeberg.org/playXE/capy/commit/2b9fba26990230212328aeaeb72501a0b003886f))
- *(tree-il)* Tree-il-fold - ([23f831c](https://codeberg.org/playXE/capy/commit/23f831c67a338fcb65ee8a45e1a0a5e6f8d6b18f))
- *(tree-il)* Define record match patterns from SRFI-257 - ([bd19b85](https://codeberg.org/playXE/capy/commit/bd19b8574806e41d3cb108f878c55fd1ce86d883))
- *(tree-il)* Add pattern matchers for IL - ([34b0430](https://codeberg.org/playXE/capy/commit/34b043015abc838acef9511327f541dd249e5c96))
- Add new SRFIs to build system and cond-expand - ([5444268](https://codeberg.org/playXE/capy/commit/5444268284acf1abf15a7a8ad1dfdcf738cc0bd3))
- Add let-optionals* to base - ([90b8b25](https://codeberg.org/playXE/capy/commit/90b8b2519cd91f54fd2a019984a4e81304fc7c69))
- Add foreign and process libraries into makefile - ([97e5c3a](https://codeberg.org/playXE/capy/commit/97e5c3a7f981b301f767b14bb5b11d557d2e9e4d))
- Add trampoline code size - ([4cfece1](https://codeberg.org/playXE/capy/commit/4cfece1cc5a4a55f964c411210760742631872a8))

### 🐛 Bug Fixes

- *(ci)* Better logic to check for recent commits - ([899c4f8](https://codeberg.org/playXE/capy/commit/899c4f891370398f667a8da4f1ebac1855218226))
- *(cps)* Disable DCE of primcalls in optimizer - ([73e27e1](https://codeberg.org/playXE/capy/commit/73e27e17c260d64560cb7deaca9b7663fa7885ad))
- *(expand)* Proper conversion of receive to Rust enum - ([dc28c8a](https://codeberg.org/playXE/capy/commit/dc28c8a1ef7305fb4425bb4e094fe2a5ac439b24))
- *(gc)* Create pinning work only when CAN_PIN_OBJECTS is #t - ([ee31692](https://codeberg.org/playXE/capy/commit/ee3169243e058089a25cc49425859851c9b82fd0))
- *(tree-il)* Fix tree-il->scheme for receive term - ([c70231d](https://codeberg.org/playXE/capy/commit/c70231d5def0d91eced9518c06e656c9e4542a41))
- *(tree-il/primitives)* Expand values to values term - ([c382724](https://codeberg.org/playXE/capy/commit/c382724eb9f4e22f5eab634d79c747d5b95afdc6))
- Warnings - ([4e2bb1e](https://codeberg.org/playXE/capy/commit/4e2bb1e28551bd57630494632af1a22fea6dd5b3))
- Cfg some constants for macOS build - ([2fd054c](https://codeberg.org/playXE/capy/commit/2fd054ceefe5a375d6f03b9a24a7b09d3313c72b))
- Quote form in lambda* - ([2eb1964](https://codeberg.org/playXE/capy/commit/2eb1964c90fd57de56927be718901509e198df65))

### 🚜 Refactor

- *(foreign)* Simplify C struct macros - ([bcb0f9b](https://codeberg.org/playXE/capy/commit/bcb0f9b97b7d3efbebd572035f9675e38fc7e29f))
- *(runime)* Store free variables inline in closure - ([652afb8](https://codeberg.org/playXE/capy/commit/652afb87554dcfe1f19cd77b42b5519efcd0dd34))
- *(runtime)* Stats and cleaner code - ([29dfb7c](https://codeberg.org/playXE/capy/commit/29dfb7cea71cf0af98002aecfb714a368d1614c9))

### ⚙️ Miscellaneous Tasks

- *(ci)* Always generate nightly changelog - ([5d63646](https://codeberg.org/playXE/capy/commit/5d6364616fe87f972984d8fa51f743991faf28d7))
- Update Makefile - ([e3978bb](https://codeberg.org/playXE/capy/commit/e3978bbac2b4dbc192f1171e11ddbd4f94526142))
- Remove unused dependencies - ([249e542](https://codeberg.org/playXE/capy/commit/249e542f366978883ae65785285fedd332775b59))
- Document blocking operation - ([73548a9](https://codeberg.org/playXE/capy/commit/73548a9f96b865799b3d793d18970b0690050216))
- Remove dead code in load.rs - ([59e032c](https://codeberg.org/playXE/capy/commit/59e032c9bd95a854a335be9226cc3d50eddd14df))
- Add fmt commit to git-blame-ignore-vars - ([a683138](https://codeberg.org/playXE/capy/commit/a683138ccc708456ebf2fe570b2afbcd9825b4fd))
- The great formatting - ([be734fa](https://codeberg.org/playXE/capy/commit/be734fadb066deb725b70b72729df149eb4591c1))

### ◀️ Revert

- Remove syscall:socket - ([02938eb](https://codeberg.org/playXE/capy/commit/02938ebb05ad65edaa9e497c3936d89901e6beaa))
- Do not use CAN_PIN_OBJECTS for bytevector - ([dfca5f0](https://codeberg.org/playXE/capy/commit/dfca5f05367a24583caf14c4f7cc6711e2bbf8c5))


## [1.1.0](https://codeberg.org/playXE/capy/compare/1.0.2..1.1.0) - 2026-01-08

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
- *(cd)* Fix title in release.yml - ([ebd40fb](https://codeberg.org/playXE/capy/commit/ebd40fbc97159daa1da504e12fa2168cfdf295b2))
- *(cd)* Do not include version in release-notes - ([9d80c10](https://codeberg.org/playXE/capy/commit/9d80c107e2b30255e4d0565c37e4f1806aef727a))
- *(ci)* Make release.yml use git cliff changelog - ([e316d9d](https://codeberg.org/playXE/capy/commit/e316d9d2a92474e1d258bf9060d7d6fe1b096ec5))
- *(ci)* Properly output release-body in release - ([403300f](https://codeberg.org/playXE/capy/commit/403300f2abca650815d45b8f039c33769f1783d1))
- *(ci)* Fix git-cliff version - ([6c3c624](https://codeberg.org/playXE/capy/commit/6c3c62492497ebedb46d0fdfb1dc2eb5c68b6976))
- *(ci)* Label names fix - ([cf5263f](https://codeberg.org/playXE/capy/commit/cf5263f1207716478f8477f6f1978ba098569844))
- *(compile-psyntax)* Compile psyntax in (capy) module - ([c3c541c](https://codeberg.org/playXE/capy/commit/c3c541ca3b3e25fd2876aaf913965023fbafc1a2))
- *(io)* Make input/output ports work - ([96ed753](https://codeberg.org/playXE/capy/commit/96ed753f4f578fe0f9929aaabb12cc94943e9657))
- Fix push of changelog - ([176f245](https://codeberg.org/playXE/capy/commit/176f2450026de6722b76bc272d3d616161f866a8))
- Changelog inclusion into release notes - ([8f0668e](https://codeberg.org/playXE/capy/commit/8f0668ed12f4f9947e30df16e0dac6a74d998346))
- Fix if condition - ([db16bee](https://codeberg.org/playXE/capy/commit/db16bee1c717e16fae89bdef13144b1887c87ed9))
- Fix yml - ([bee9cb4](https://codeberg.org/playXE/capy/commit/bee9cb4283622c149560175a7d77acc25534c792))
- Allow R6RS let-syntax with empty body - ([7a7c163](https://codeberg.org/playXE/capy/commit/7a7c163412523d94a65de0f8ed071d720cda7bc9))
- Make empty string comparison work - ([0bde187](https://codeberg.org/playXE/capy/commit/0bde187375f5b1ab493c4a8706e5195ca9c048c2))
- Write flonums with dot included - ([d9a5ef0](https://codeberg.org/playXE/capy/commit/d9a5ef074bd80adffdf0c267b554b7c8e642b4af))

### 🚜 Refactor

- *(capy)* Formatting - ([f90e0a7](https://codeberg.org/playXE/capy/commit/f90e0a78664ceb47c744402e71dd8deaf7ebd4f7))
- *(ci)* Mark nightly release as pre-release - ([f1351b9](https://codeberg.org/playXE/capy/commit/f1351b98207b9ebd61a5f62d05e8dd071ca620ab))
- Split tree-il module into multiple files - ([a8d087c](https://codeberg.org/playXE/capy/commit/a8d087c0354d547c53434d06de640f3a5f6fdeb8))


## [1.0.2](https://codeberg.org/playXE/capy/compare/1.0.1..1.0.2) - 2026-01-04

### 🐛 Bug Fixes

- Fix rpm meta - ([fc628e7](https://codeberg.org/playXE/capy/commit/fc628e79de593077e3d2e22416d4f12a10426180))
- Fix rpm name - ([27baa0c](https://codeberg.org/playXE/capy/commit/27baa0cca390055606215d47e2be0042740e0033))
- Fix package name - ([0b0e28c](https://codeberg.org/playXE/capy/commit/0b0e28cb1bd48cbb64ce3c1cc4076f5d6d39515a))
- Fix upload of packages - ([de0713c](https://codeberg.org/playXE/capy/commit/de0713c9f8dcad9bb0c64cc189d2b65125cd532f))


## [1.0.1](https://codeberg.org/playXE/capy/compare/v1.0.1..1.0.1) - 2026-01-04


## [1.0.1](https://codeberg.org/playXE/capy/compare/v1.0.0..v1.0.1) - 2026-01-03

### 🐛 Bug Fixes

- Fix build job - ([e7b2acf](https://codeberg.org/playXE/capy/commit/e7b2acff94ee89d7867ee6b3ff807890ba5ebf17))
- Fix - ([0435653](https://codeberg.org/playXE/capy/commit/04356531872482bba41c87a7dbfa97ea32abb45e))

### ◀️ Revert

- Revert panic - ([21d42de](https://codeberg.org/playXE/capy/commit/21d42de296c0d0da0d4fa434d9890a23e7389d5a))
- Revert opt changes - ([7fa17d0](https://codeberg.org/playXE/capy/commit/7fa17d0a80702b5c99de566d9780d9c27a094874))


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
- Fix env in makefile; update psyntax - ([ee487b7](https://codeberg.org/playXE/capy/commit/ee487b7fded88304de7f6ceb285cce8c1d3126ef))
- Fix false-if-exception - ([b3fd437](https://codeberg.org/playXE/capy/commit/b3fd437b9f417c31527992a4ab205b38f30a3d41))
- Fix path again - ([a4257fc](https://codeberg.org/playXE/capy/commit/a4257fc6fca453da534eb8d0c5b443dac7d97276))
- Fix dead code - ([f25639e](https://codeberg.org/playXE/capy/commit/f25639ed82cabc06b5abbaa4c32654193879f3ba))
- Fix runner name - ([951169f](https://codeberg.org/playXE/capy/commit/951169f1f5664466636694c501124d42479a84d7))
- Fix %load-path - ([24d9470](https://codeberg.org/playXE/capy/commit/24d94705e525e98fbaa9198345db79b7fde30a85))
- Fix - ([a6cda56](https://codeberg.org/playXE/capy/commit/a6cda56b904325d45528cc546aaa20189f8ffdeb))
- Fix `-` - ([6dcdd79](https://codeberg.org/playXE/capy/commit/6dcdd79a3f2e114cf66dad2f7640d1691e661fd4))
- Once candidate found break out of search loop properly - ([68c5f98](https://codeberg.org/playXE/capy/commit/68c5f98a9e3a917df447b1ae1d0192eec6a4c540))
- Fix prefix install - ([e1b000d](https://codeberg.org/playXE/capy/commit/e1b000d2c88101477849a0bfb2dad4608d09792b))
- Fix eta-expansion - ([73371ec](https://codeberg.org/playXE/capy/commit/73371ec4bba0ab6ea19e7cade226e6b9ba054afe))
- Fix macos build - ([1ade1f3](https://codeberg.org/playXE/capy/commit/1ade1f388824736614aeb88e2850efad2dde7466))
- Fix (srfi 39) imports - ([c09bce8](https://codeberg.org/playXE/capy/commit/c09bce824d2e0155a5102a904401799a639f971c))
- Fix imports - ([8b9f4c8](https://codeberg.org/playXE/capy/commit/8b9f4c8a7a6a0ccec72742d95ff3c8f6c2748276))
- Fix linux build - ([9a6b41d](https://codeberg.org/playXE/capy/commit/9a6b41d5fd8d86938f9416caaf07d3fd165076fa))
- Fix macOS build - ([6fe4976](https://codeberg.org/playXE/capy/commit/6fe4976f5f3bc138853f4ce67f757bf3c6db6fd3))
- Fix fix_letrec.rs - ([7a0f671](https://codeberg.org/playXE/capy/commit/7a0f6714ea99b9120b8515522c2ba27734f719ae))
- Fix logging - ([1a87216](https://codeberg.org/playXE/capy/commit/1a8721679b846e3830574765efdda86e294e6310))
- Fix fxarithmetic-shift - ([b605895](https://codeberg.org/playXE/capy/commit/b6058957137d74fe885e5bb67f566687184d5b3b))
- Fix build - ([eacb38b](https://codeberg.org/playXE/capy/commit/eacb38b4515f0043760aa3e6834192a666b43fc9))
- Fix rtd-ancestor? - ([c83aedf](https://codeberg.org/playXE/capy/commit/c83aedf31a8abf9b91b1fb00fb99d15e62d5ee0d))
- Fix string->utf16/utf32, fix syntax-violation - ([7937dcb](https://codeberg.org/playXE/capy/commit/7937dcbdda6e3f645fb523c13d2d01331821e00d))
- Fix typo - ([553d2ae](https://codeberg.org/playXE/capy/commit/553d2ae4c3d8694d1c416efdd9b5a3d2be2476ad))
- Fix build - ([1ee8f0d](https://codeberg.org/playXE/capy/commit/1ee8f0d012f026523a4a5786d3bf61bd7a9d5e73))
- Fix most of miscompilations - ([4d7ff5c](https://codeberg.org/playXE/capy/commit/4d7ff5cb3403b82ebd061a0e9a39d6da7aa577d7))
- Fix topbox scope usage in CPS transform - ([6c6cda7](https://codeberg.org/playXE/capy/commit/6c6cda7de3ac1586bea9951262b6fe1830aa0607))
- Disable reordering for now - ([cec06ff](https://codeberg.org/playXE/capy/commit/cec06ff76a8d624cfb08f9395fa88426f24e7bcd))
- Add version to libraries - ([a78ff2a](https://codeberg.org/playXE/capy/commit/a78ff2a5ce89b282544bac34b7c35232d0525107))
- Fix extension list - ([7bb7fa4](https://codeberg.org/playXE/capy/commit/7bb7fa469bd2090dfd1f06698fad4d7a0981ca1f))
- Fix expansion of some primitives - ([da87252](https://codeberg.org/playXE/capy/commit/da8725250fa2c6444db85751ca3ab1c912948a13))
- Fix module cache - ([c8d2886](https://codeberg.org/playXE/capy/commit/c8d2886832af608abed615718f8ed5529abca90f))
- Fix print - ([920aaa1](https://codeberg.org/playXE/capy/commit/920aaa1d4662c820a5d23716035d171527bc9c9d))
- Fix dynamic-wind - ([4d31372](https://codeberg.org/playXE/capy/commit/4d31372893fdd283f53f31c98d5bc24b55385d87))
- Fixing bugs - ([5b02f5e](https://codeberg.org/playXE/capy/commit/5b02f5e466c0689acaf152fabf7f8c2577d8b011))
- Fix contification - ([7dc8dcf](https://codeberg.org/playXE/capy/commit/7dc8dcf70cb9fd3b5f763af986759b8d0c6b36bb))

### 🚜 Refactor

- *(runtime)* Make modules declarative by default - ([a2899b1](https://codeberg.org/playXE/capy/commit/a2899b18c83c2bb073aa3572d06d8f52fd874732))

### ⚡ Performance

- Perform yieldpoint instead of triggering GC - ([5087ea8](https://codeberg.org/playXE/capy/commit/5087ea8b2b979e3e22cb6b8249adb9993c8bbbc3))

### ◀️ Revert

- Revert match-syntax - ([9586f3f](https://codeberg.org/playXE/capy/commit/9586f3f6eaba7d329dc29cef9639740f6b90ba12))

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
