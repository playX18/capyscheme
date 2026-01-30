## [unreleased]

### ⛰️  Features

- *(capyc)* Add --nobacktrace flag - ([8185020](https://codeberg.org/playXE/capy/commit/8185020315558d2f940781dbe4b56c1b35f3a8c9))
- *(core/foreign)* Use keyword arguments - ([30f707d](https://codeberg.org/playXE/capy/commit/30f707d8c5130011e44a91ae3803bcd3f0111d14))
- *(ffi)* Add blocking calls support - ([bf23f49](https://codeberg.org/playXE/capy/commit/bf23f495b91866660d9a1896a644146b605e967f))
- *(gc)* Pinning roots - ([bf8a1af](https://codeberg.org/playXE/capy/commit/bf8a1af2b2c3e4c0b49107eb382824dd835bacaa))
- *(runtime)* Add Socket type - ([149a4d3](https://codeberg.org/playXE/capy/commit/149a4d35b08c931c5c35763f928aa1012a88a2db))
- *(runtime)* Use self-hosted primitive expansion after bootstrap - ([82c55a7](https://codeberg.org/playXE/capy/commit/82c55a76aa03b9f6e9211c79245380458e143f6f))
- *(srfi-257)* Simple extendable pattern matcher with backtracking - ([2c435b4](https://codeberg.org/playXE/capy/commit/2c435b477755665647079a14da566dde96448da1))
- *(ssa)* Add (unspecified) lowering - ([2b9fba2](https://codeberg.org/playXE/capy/commit/2b9fba26990230212328aeaeb72501a0b003886f))
- *(tree-il)* Tree-il-fold - ([23f831c](https://codeberg.org/playXE/capy/commit/23f831c67a338fcb65ee8a45e1a0a5e6f8d6b18f))
- *(tree-il)* Define record match patterns from SRFI-257 - ([bd19b85](https://codeberg.org/playXE/capy/commit/bd19b8574806e41d3cb108f878c55fd1ce86d883))
- *(tree-il)* Add pattern matchers for IL - ([34b0430](https://codeberg.org/playXE/capy/commit/34b043015abc838acef9511327f541dd249e5c96))
- Add let-optionals* to base - ([90b8b25](https://codeberg.org/playXE/capy/commit/90b8b2519cd91f54fd2a019984a4e81304fc7c69))
- Add foreign and process libraries into makefile - ([97e5c3a](https://codeberg.org/playXE/capy/commit/97e5c3a7f981b301f767b14bb5b11d557d2e9e4d))
- Add trampoline code size - ([4cfece1](https://codeberg.org/playXE/capy/commit/4cfece1cc5a4a55f964c411210760742631872a8))

### 🐛 Bug Fixes

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

### ⚙️ Miscellaneous Tasks

- Update CHANGELOG.md [skip ci] - ([55b7418](https://codeberg.org/playXE/capy/commit/55b74184386897a009ee9eef23721facb434a0fc))
- Update CHANGELOG.md [skip ci] - ([944336e](https://codeberg.org/playXE/capy/commit/944336e391a612a628ce8b0959d8a5f5d5d6c7ee))
- Update CHANGELOG.md [skip ci] - ([eaa5313](https://codeberg.org/playXE/capy/commit/eaa5313070f04d6239cea6c849a38b72560bd898))
- Update CHANGELOG.md [skip ci] - ([1cef306](https://codeberg.org/playXE/capy/commit/1cef306e18d049621051ba9b184c31001517c4fb))
- Update CHANGELOG.md [skip ci] - ([32bf43e](https://codeberg.org/playXE/capy/commit/32bf43e62fda1dcf970d8398549102e10ea2c608))
- Update CHANGELOG.md [skip ci] - ([af57399](https://codeberg.org/playXE/capy/commit/af57399f57aa45cb0ee46fb6de3390b3210ad871))
- Update CHANGELOG.md [skip ci] - ([3948bfc](https://codeberg.org/playXE/capy/commit/3948bfc412d65aee6302bbbb68dc8e68c32395cf))
- Update CHANGELOG.md [skip ci] - ([d640d21](https://codeberg.org/playXE/capy/commit/d640d21c44d960f0324963c461c632ae715259a7))
- Update CHANGELOG.md [skip ci] - ([20ee95e](https://codeberg.org/playXE/capy/commit/20ee95e70df2e14ee2002cf0ea2011e4455db56c))
- Update CHANGELOG.md [skip ci] - ([accfa23](https://codeberg.org/playXE/capy/commit/accfa236fce5523cd964e0a971e150d8f9015baf))
- Update CHANGELOG.md [skip ci] - ([a491bb4](https://codeberg.org/playXE/capy/commit/a491bb49a240386dba80e5f796bb86dd2564ff40))
- Update CHANGELOG.md [skip ci] - ([53b8e47](https://codeberg.org/playXE/capy/commit/53b8e47a9a84e2083d17e4df0338fb6fc8c1aaa6))
- Update CHANGELOG.md [skip ci] - ([168da6b](https://codeberg.org/playXE/capy/commit/168da6b368939fba0d2f706087ea11961c28b0b4))
- Update CHANGELOG.md [skip ci] - ([3fa17cc](https://codeberg.org/playXE/capy/commit/3fa17ccaebc16fc1ad9fdc25e0f9e4ed1811e638))
- Update CHANGELOG.md [skip ci] - ([5e97926](https://codeberg.org/playXE/capy/commit/5e979264e5ca523d1895a77d1d7e62003d5f96a2))
- Update CHANGELOG.md [skip ci] - ([685a71a](https://codeberg.org/playXE/capy/commit/685a71a503a9bf5273bfe5176331eca7add73e81))
- Update CHANGELOG.md [skip ci] - ([3afd573](https://codeberg.org/playXE/capy/commit/3afd57316dbad9419647020c3515215b77f54139))
- Update CHANGELOG.md [skip ci] - ([617862c](https://codeberg.org/playXE/capy/commit/617862c90704b5fee1ff2535e57de268763f22a5))
- Update CHANGELOG.md [skip ci] - ([78063d4](https://codeberg.org/playXE/capy/commit/78063d46b20764d436fb56275eec1f3764235e06))
- Update CHANGELOG.md [skip ci] - ([f00e169](https://codeberg.org/playXE/capy/commit/f00e1694fea81ea5b18f1079f2208d72476d15ca))
- Update CHANGELOG.md [skip ci] - ([be53eb1](https://codeberg.org/playXE/capy/commit/be53eb17a211a1cee849774bb481a145238cddbf))
- Update CHANGELOG.md [skip ci] - ([bed40ba](https://codeberg.org/playXE/capy/commit/bed40ba7f66e25777912bd76a6734f5ec20f681e))
- Remove unused dependencies - ([249e542](https://codeberg.org/playXE/capy/commit/249e542f366978883ae65785285fedd332775b59))
- Document blocking operation - ([73548a9](https://codeberg.org/playXE/capy/commit/73548a9f96b865799b3d793d18970b0690050216))
- Remove dead code in load.rs - ([59e032c](https://codeberg.org/playXE/capy/commit/59e032c9bd95a854a335be9226cc3d50eddd14df))
- Update CHANGELOG.md [skip ci] - ([b03c591](https://codeberg.org/playXE/capy/commit/b03c59178b2140e8d8504bac8aafec6bea2a9459))
- Update CHANGELOG.md [skip ci] - ([490d19f](https://codeberg.org/playXE/capy/commit/490d19f713a6c409470a0e214a62632f4ec6dea7))
- Add fmt commit to git-blame-ignore-vars - ([a683138](https://codeberg.org/playXE/capy/commit/a683138ccc708456ebf2fe570b2afbcd9825b4fd))
- The great formatting - ([be734fa](https://codeberg.org/playXE/capy/commit/be734fadb066deb725b70b72729df149eb4591c1))
- Update CHANGELOG.md [skip ci] - ([be6eaec](https://codeberg.org/playXE/capy/commit/be6eaecb77e028db71ef0d63a7f977c774a4b1aa))
- Update CHANGELOG.md [skip ci] - ([10407cd](https://codeberg.org/playXE/capy/commit/10407cdb9c10ee0ab649730735efab8f9ed312f9))

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

### ⚙️ Miscellaneous Tasks

- Update CHANGELOG.md [skip ci] - ([4b87e26](https://codeberg.org/playXE/capy/commit/4b87e262b448ba8d53f9c2ce0e88a1b7ff2177f1))
- Update CHANGELOG.md [skip ci] - ([d697884](https://codeberg.org/playXE/capy/commit/d69788421434b40f27e5cbcbdf37e09f918e1290))
- Update CHANGELOG.md [skip ci] - ([7a89f6a](https://codeberg.org/playXE/capy/commit/7a89f6af4704c90351a1e4617e3d97449651d65b))
- Update CHANGELOG.md [skip ci] - ([507fb18](https://codeberg.org/playXE/capy/commit/507fb18c8b5730a3f598d187031f02deb2132cb6))
- Update CHANGELOG.md [skip ci] - ([301eb81](https://codeberg.org/playXE/capy/commit/301eb819d1e218137ec26753d35bfbb6da3f2815))
- Update CHANGELOG.md [skip ci] - ([b66a83f](https://codeberg.org/playXE/capy/commit/b66a83faf31b9d1c1e37dac0aba4c9b8fd420c44))
- Update CHANGELOG.md [skip ci] - ([d321355](https://codeberg.org/playXE/capy/commit/d321355e8828a230bbcacff544b0e8cc20958299))
- Update CHANGELOG.md [skip ci] - ([8b55330](https://codeberg.org/playXE/capy/commit/8b5533092b4e9986d1aa82822cf7d3daaaa7c94e))
- Update CHANGELOG.md [skip ci] - ([c4e3175](https://codeberg.org/playXE/capy/commit/c4e3175480e4c5da6eabb7996187c6828d19d5d5))
- Update CHANGELOG.md [skip ci] - ([16d81bc](https://codeberg.org/playXE/capy/commit/16d81bcd2b9549a142ba6d8051b06a5112f06f94))


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
