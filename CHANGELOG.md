## [2.0.0](https://codeberg.org/playXE/capy/compare/v1.3.0..v2.0.0) - 2026-06-25

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
- *(ci)* Add test running on push and PR - ([c8686a4](https://codeberg.org/playXE/capy/commit/c8686a440a3ab279c46cec73bf966756b18caef3))
- *(class)* Add runtime object system - ([4c18988](https://codeberg.org/playXE/capy/commit/4c18988d1574a88e26844d4eea1f6de3f2c8d9b7))
- *(compiler)* Optimize epilogues - ([5e4a413](https://codeberg.org/playXE/capy/commit/5e4a41330a456fe09f9d99562505960d4f684a30))
- *(compiler)* Better ABI - ([081e51c](https://codeberg.org/playXE/capy/commit/081e51c677b2092f2169681b4dcc16156f0b6957))
- *(compiler)* Optimize rest arguments to direct access - ([76e7a05](https://codeberg.org/playXE/capy/commit/76e7a056cf0b938fd37a38e2bdd3e24503cb7f81))
- *(control)* Implement call-in-continuation and add tests for its behavior - ([7fce732](https://codeberg.org/playXE/capy/commit/7fce732bfd1228654c1899cf1c8c0424be0882f8))
- *(core)* Fancy repl - ([9f3b8b3](https://codeberg.org/playXE/capy/commit/9f3b8b38e2ce9924c47b41a92ceeb1d88b7b4534))
- *(ffi)* Add callbacks - ([5239967](https://codeberg.org/playXE/capy/commit/5239967edc88240e453b8b4a9402578ab155ba85))
- *(lcps)* Low level primops - ([ee6aa09](https://codeberg.org/playXE/capy/commit/ee6aa09f8126ae8741a8a23627393497e9787233))
- *(lib)* Add terminal library - ([4107b51](https://codeberg.org/playXE/capy/commit/4107b5177d9cdb624dcb40336638bbe9dcc012e5))
- *(library)* Auto-import capy prelims - ([3116a3f](https://codeberg.org/playXE/capy/commit/3116a3f471f7f1bf229a60216029ef0b2b09bc38))
- *(lsp)* Dsl docstring - ([a07efec](https://codeberg.org/playXE/capy/commit/a07efec53c580c0df346669a90a1da5882417352))
- *(lsp)* One shot workers - ([c9c26a2](https://codeberg.org/playXE/capy/commit/c9c26a227528e9d42e112b82b4fac89b0855529d))
- *(lsp)* Enhance hover functionality and add import metadata support - ([529e860](https://codeberg.org/playXE/capy/commit/529e8604b61920530b978747c8dcefd102796934))
- *(lsp)* Build workspace index on worker side - ([88fd7fc](https://codeberg.org/playXE/capy/commit/88fd7fc4f760ab5a9efc78fc0851e15b999336ee))
- *(lsp)* CallGraph - ([a7d2ebe](https://codeberg.org/playXE/capy/commit/a7d2ebec1d75a3fe8aa3051aa2fb2dd1e7fb66e5))
- *(lsp)* Add refactored Rust language server - ([12382c8](https://codeberg.org/playXE/capy/commit/12382c8d29247d2409d7ce9dfd9a097a27fdfed5))
- *(lsp)* Add Scheme worker analysis - ([689d7aa](https://codeberg.org/playXE/capy/commit/689d7aa581242527135bd060d1343be4f4c59b3b))
- *(lsp)* Add isolated VM worker launcher - ([36d267b](https://codeberg.org/playXE/capy/commit/36d267b735921bae7da68db6644565f45427b7c8))
- *(psyntax)* Move library macros to psyntax - ([85cb714](https://codeberg.org/playXE/capy/commit/85cb714c0675e49af5f35c1d89295ac866b11bdb))
- *(psyntax)* Add default core #%app - ([0c8d46c](https://codeberg.org/playXE/capy/commit/0c8d46c48f3fdf275acdc2a0dad6a034f6416dd7))
- *(psyntax)* Route calls through implicit #%app - ([4192658](https://codeberg.org/playXE/capy/commit/419265893227c2a8f56a7ecebdce8fee1fcee9a1))
- *(psyntax)* Define-property - ([1dd6932](https://codeberg.org/playXE/capy/commit/1dd69323e87e691da282e613fb03ed5b8c0cd50c))
- *(resolve-free-vars)* Implement free variable resolution in tree-il compiler - ([47f0081](https://codeberg.org/playXE/capy/commit/47f0081aecd2a628e5d19c3d166f6e81bc5a8e78))
- *(runtime)* Add term low-level functionns - ([4104c51](https://codeberg.org/playXE/capy/commit/4104c51ff2ba7ed6e9322246bad6e78a9aaf73e2))
- *(runtime, compiler)* Tail call into error thunks instead of calls - ([636d9ce](https://codeberg.org/playXE/capy/commit/636d9ce9a7a2d4e027fa93ba3c4774d58384271b))
- *(srfi)* Add custom comparator to SRFI-64 - ([1ea56e3](https://codeberg.org/playXE/capy/commit/1ea56e3278a80df9eb397fb3f02b50d0ea1f6a36))
- *(srfi)* SRFI-213 full implementation - ([db8ebc7](https://codeberg.org/playXE/capy/commit/db8ebc7df8a292b6d1a9a30c692a265cb36cb9d8))
- *(ssa)* Inline allocation - ([1fd7b1b](https://codeberg.org/playXE/capy/commit/1fd7b1b2845547cacbdd03ca5fcd7aa74c4ccd91))
- *(tests)* Use srfi-64 test runner for racket/r6rs test suite - ([0ca56c9](https://codeberg.org/playXE/capy/commit/0ca56c9ed305d6528c9c56be31d37a483dd74fba))
- *(vscode)* Add Capy Scheme LSP extension - ([180ec16](https://codeberg.org/playXE/capy/commit/180ec160da7aa46f8b6c981d6a59961ef96e036f))
- Use generic static for classID registratio (fixed) - ([d0651b5](https://codeberg.org/playXE/capy/commit/d0651b58aab44eedef6c318c08be077c98d9d98e))
- Use generic static to allocate class ids - ([28bb3aa](https://codeberg.org/playXE/capy/commit/28bb3aabca763f4fd71ad4d73d50d336ca4a09b0))
- Make slot-ref-using-class and slot-set-using-class generics - ([d9fbe3d](https://codeberg.org/playXE/capy/commit/d9fbe3d15cedc8f54e7c176f2e259297cc61c84f))
- Faster class ID allocation - ([ee5bea6](https://codeberg.org/playXE/capy/commit/ee5bea63f6eb59b87b0ced518c538bdaa03d1f63))
- FASL full encoding - ([702d5ce](https://codeberg.org/playXE/capy/commit/702d5ce2c05b79feb5e7f8767568faddcd619516))
- FASL compilation instead of ELF/mach-o - ([4eb8e94](https://codeberg.org/playXE/capy/commit/4eb8e94a296091b6cde01ae13dbe3f61cfa55833))
- Thread interrupts - ([d7c5768](https://codeberg.org/playXE/capy/commit/d7c5768c276294cc8f11861a351a1a02121de33e))
- Add fault-driven yieldpoints - ([f8d63cd](https://codeberg.org/playXE/capy/commit/f8d63cda3e270380594cca098107be47531216ac))
- Direct style advancements - ([675d0a7](https://codeberg.org/playXE/capy/commit/675d0a7f3eae3554e1c39764b4f4ed6c0c2b9171))
- Conservative stack scanning - ([96ce46d](https://codeberg.org/playXE/capy/commit/96ce46dc9d9bcae80d9050278f7aa9245157e9f7))
- Stop wasting time on building Rust code unless there are changes - ([9a3c0d2](https://codeberg.org/playXE/capy/commit/9a3c0d273b7159c9a3713e7739dbf32b9c42fe0f))
- Add cache lock - ([499ad32](https://codeberg.org/playXE/capy/commit/499ad32545131a1168e7d4326b2870f88600ed26))
- Letrectify in self-hosted compiler - ([99f3001](https://codeberg.org/playXE/capy/commit/99f30016026c1e107b2f74299137623db5f11e99))
- Use tree il as source of truth for lsp - ([842dc7a](https://codeberg.org/playXE/capy/commit/842dc7a1a4c6e343952aa8ebb83196eb70537780))

### 🐛 Bug Fixes

- *(capy/gc)* Mutator always used 8 bytes alignment, use max(alignment, 8) - ([577c283](https://codeberg.org/playXE/capy/commit/577c283c91b61dcb7b7e930437838aa359542f7d))
- *(capy/term)* Synchronized update - ([ea0f259](https://codeberg.org/playXE/capy/commit/ea0f259a2234421d0cb246cc7b7e2ad6efaa2c7e))
- *(core)* Make generic hashtables work properly - ([3becc08](https://codeberg.org/playXE/capy/commit/3becc08906b43fb0835fb84fe45c23b7280dc82b))
- *(cps)* Lower to wrong arity condition instead of panicking in compiler - ([4ecdeca](https://codeberg.org/playXE/capy/commit/4ecdecaa0f4c1a929fe339b4aa8ea12d756d7c6e))
- *(import)* Resolve libraries with numbers properly - ([c9cbf35](https://codeberg.org/playXE/capy/commit/c9cbf35b4acb5fda00163b2f88ad377d2a1ccbd2))
- *(lib)* Fix #%app imports - ([23d5085](https://codeberg.org/playXE/capy/commit/23d50859053535873404050e35ccee128280173d))
- *(lib/capy)* Better threading - ([712093f](https://codeberg.org/playXE/capy/commit/712093f4a25348acec0393f9325d51d6814b046b))
- *(lib/core)* Mutex protect data - ([e551a91](https://codeberg.org/playXE/capy/commit/e551a911a1704e6744ff456c2491a27dfa2d1742))
- *(number)* Fix quotient on flonums panicking - ([f59029c](https://codeberg.org/playXE/capy/commit/f59029c9bf6c4559f57ad1a8c928468777532a1e))
- *(psyntax)* Splice begin body in define-library correctly - ([43663b7](https://codeberg.org/playXE/capy/commit/43663b7e30d610a5040c971ae3401e772b8e56b1))
- *(psyntax)* Proper expadn-expr on eval-when - ([b890104](https://codeberg.org/playXE/capy/commit/b890104e830b00b482439d5cdbe5db272ed2d7f0))
- *(psyntax)* Stabilize implicit #%app bootstrap - ([5df6bc0](https://codeberg.org/playXE/capy/commit/5df6bc03e6114e320377034c9bf6496c2f03d325))
- *(repl)* Import core control - ([e29aaab](https://codeberg.org/playXE/capy/commit/e29aaab17ef7bd54a3f0e304a6894315de33b8af))
- *(tests)* Letrectify outputs letrec* - ([8b6ebf5](https://codeberg.org/playXE/capy/commit/8b6ebf58318f8131c2d752bea2fd8561cd9d49f5))
- Load compiler.scm in boot - ([b2fd466](https://codeberg.org/playXE/capy/commit/b2fd46642da82e8b7f93ca295a6c165aed877790))
- Mutex? re-export, better debugging - ([e315336](https://codeberg.org/playXE/capy/commit/e315336f442998daf1f94339b36667b746501fde))
- Use lockfile for cache files - ([6e80d87](https://codeberg.org/playXE/capy/commit/6e80d87ad728a9cbd7a714b2703083ac01d23364))
- Better ABI, simplify safepotints, fix negation - ([f85f609](https://codeberg.org/playXE/capy/commit/f85f60996f20278ec50ee00bb4029df6cadc8dc4))
- Conservative scanning - ([5f6926b](https://codeberg.org/playXE/capy/commit/5f6926b23ab40d916010900b33382738ababc18c))
- Fix keywords - ([377b117](https://codeberg.org/playXE/capy/commit/377b117ae77c8772e1aa3160cb2e5097abaafc76))
- Print subform from syntax-violation correctly - ([45c8c4e](https://codeberg.org/playXE/capy/commit/45c8c4e41bbf68b73c34a47a95dc9280426c41a4))
- Empty values now report arity error where applicable instead of silent crash - ([c5704cb](https://codeberg.org/playXE/capy/commit/c5704cb3d047d796d559baa5a9266bb78b0f9b30))
- Dead code - ([2579e97](https://codeberg.org/playXE/capy/commit/2579e972148f3b19c5aec6575dd2b6104e4fa81f))
- Remove redundant/outdated tests - ([47c0fd4](https://codeberg.org/playXE/capy/commit/47c0fd495b645fe08a0d1476eb860ad0d982cffc))
- Remove outdated/redundant tests - ([0bac374](https://codeberg.org/playXE/capy/commit/0bac374af19e7daf60fab38a3538fcb01b04bc0e))
- Remove cached repo - ([cae021d](https://codeberg.org/playXE/capy/commit/cae021dbeacba2e891cc9d38cd0b1e4033e0044c))

### 🚜 Refactor

- *(capy)* Simplify compiler - ([5f0402a](https://codeberg.org/playXE/capy/commit/5f0402aee82ce54dde97ff24767a86c1bf5236ea))
- *(capy)* Better CPS memory usage - ([3f3b0ff](https://codeberg.org/playXE/capy/commit/3f3b0ff99fafa622814e7df026d44f9a0ac8c8ee))
- *(capy)* Infer switches even if block has more than 2 opcodes - ([14040be](https://codeberg.org/playXE/capy/commit/14040be4c9394ffff7242763a8380080196f4e34))
- *(capy/gc)* Split heuristics into files; better adaptive heuristic - ([ffdfdac](https://codeberg.org/playXE/capy/commit/ffdfdacf19be683c978d0ab50b8ddaa365e51c0d))
- *(compiler)* Use ValueId in linear CPS - ([e6b26bd](https://codeberg.org/playXE/capy/commit/e6b26bd224ad526b505e4ddb76ecd41c9fa6d0ed))
- *(lsp)* Restart on file changes instead of fighting with autoload - ([67e3cb8](https://codeberg.org/playXE/capy/commit/67e3cb8ae1302353deff8f567a6b01c6a124c5b8))
- *(lsp)* Invalidate files differently - ([1e1b74c](https://codeberg.org/playXE/capy/commit/1e1b74c0cfd9311fdf0d22532371821e546ac08d))
- *(runtime)* Better source handling - ([83a33e2](https://codeberg.org/playXE/capy/commit/83a33e2a32f53303269c26fc022291a2b975d3f6))
- Split up files, safety docs, improve code style - ([badf1bc](https://codeberg.org/playXE/capy/commit/badf1bc27121db10e7159bb0ef5f6c750d536f40))
- Remove dead code and perform formatting - ([cce3edd](https://codeberg.org/playXE/capy/commit/cce3edd9d9da748c1a6bb9ee561fbd095a850001))
- Split class.rs - ([2157def](https://codeberg.org/playXE/capy/commit/2157def919cce404908c6f4c23317c1630f2a93a))
- Simplify codegen and fasl - ([51f61c1](https://codeberg.org/playXE/capy/commit/51f61c181dcc766d82773e05f36b54494f3e2546))
- #!nobacktrace in letrectify and resolve-free-vars - ([7365caa](https://codeberg.org/playXE/capy/commit/7365caa3337836c785ce22cc125e8a728d698b60))

### 📚 Documentation

- Design linear cps ssa refactor - ([efa3c72](https://codeberg.org/playXE/capy/commit/efa3c72b9813c20772d72116a11038e9132cfaf6))
- Design cps block macro - ([95ac1d4](https://codeberg.org/playXE/capy/commit/95ac1d40fdc36b4a014b6d0d517184935aa12919))

### 🧪 Testing

- Add generic hashtable test to r6rs test suite - ([d3ed8ba](https://codeberg.org/playXE/capy/commit/d3ed8bac49a727b1cfc6dcd55b3d113d45443c54))
- Cover implicit #%app expansion - ([b6f2633](https://codeberg.org/playXE/capy/commit/b6f263318e24bedb7556b847e6e0dcde8422b13d))

### ⚙️ Miscellaneous Tasks

- *(clippy)* Remove lint suppressions - ([835a163](https://codeberg.org/playXE/capy/commit/835a16370d6bd3935c34575c8f065a3c9a90a9a4))
- Prepare v2.0.0 release - ([da71017](https://codeberg.org/playXE/capy/commit/da71017674db46f96e65c8be35ba8b889e283d4b))
- Cleanup module loading and tests - ([54341f8](https://codeberg.org/playXE/capy/commit/54341f81d54546af5c7b435f5490fb04363e4e7b))
- Update BOOTSTRAP.md - ([762c830](https://codeberg.org/playXE/capy/commit/762c8308813076eb4d537da945ef15c8dc87f450))
- Simplify fasl - ([77cb2ba](https://codeberg.org/playXE/capy/commit/77cb2ba822ff85d2437494000f14826085ca6c9f))
- Support FASL boostrap - ([75cbdc7](https://codeberg.org/playXE/capy/commit/75cbdc7301c71abcaad74a47153c46d40e7678ff))
- Fmt - ([d4fcac4](https://codeberg.org/playXE/capy/commit/d4fcac4e40ec677a89d5cb6356f30f26eca99b25))
- Remove dead code - ([635aa02](https://codeberg.org/playXE/capy/commit/635aa0211d8a5548c011e2e6dcca060c0eff6e98))
- Remove bytecode module - ([5eea835](https://codeberg.org/playXE/capy/commit/5eea83549d57aa3a70b5f1e725d2f594ad6162b4))
- Ignore worktree directory - ([e9d3214](https://codeberg.org/playXE/capy/commit/e9d3214f42b9da9dbb23c18536d37c50febbef26))

### ◀️ Revert

- *(capy/cps)* Optimization caused bugs in bootstrap - ([19e16cf](https://codeberg.org/playXE/capy/commit/19e16cf0a3e320bdbfac8904f8131f977daf08de))
- Stop checking the whole class hierarchy for primitvies - ([5d9d2a4](https://codeberg.org/playXE/capy/commit/5d9d2a4ad6fee08d61dc25a9a7c106db805d3f5c))

### Build

- Regenerate psyntax for implicit #%app - ([72523ed](https://codeberg.org/playXE/capy/commit/72523edc300ce198e81f0ad32420398622fc4c3d))

### Refator

- *(capy/gc)* Measure GC start/end using GCTriggerPolicy - ([90f48b5](https://codeberg.org/playXE/capy/commit/90f48b5fd7d7ebbd12464a8f891b37d672b33113))


<!-- generated by git-cliff -->
