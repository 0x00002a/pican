# Pican

## Introduction

Early stages of a rust toolchain for pica200 shaders in the [picasso] assembly format


## Usage

`picanc` is the cli, it hooks into the `pican` library to act as an assembler, disassembler, and linter. Do `cargo -p picanc -- --help` for usage instructions


## Feature parity status

### Directives

- [X] .proc
- [X] .else
- [X] .end
- [X] .alias
- [X] .fvec
- [X] .ivec
- [X] .bool
- [X] .constf
- [X] .consti
- [ ] .constfa
- [X] .in
- [X] .out
- [ ] .entry
- [ ] .nodvle
- [ ] .gsh
- [ ] .setf
- [ ] .seti
- [ ] .setb

### Instructions

- [X] nop
- [X] end
- [X] add
- [X] dp3
- [X] dp4
- [X] dph
- [X] rsq
- [X] mul
- [X] sge
- [X] slt
- [X] max
- [X] min
- [X] ex2
- [X] lg2
- [ ] litp
- [X] flr
- [X] rcp
- [X] mov
- [X] mova
- [ ] call
- [ ] for
- [ ] break
- [ ] breakc
- [X] ifc
- [ ] jmpc
- [ ] callu
- [ ] ifu
- [ ] jmpu
- [X] mad



[picasso]: https://github.com/devkitPro/picasso