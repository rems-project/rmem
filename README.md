# rmem: Executable concurrency models for ARMv8, RISC-V, Power, and x86

The rmem tool comprises executable operational models for the
relaxed-memory concurrency semantics of the Power, ARMv8, RISC-V, and
x86 (TSO) processor architectures, as well as machinery for executing
the models on litmus tests and ELF binaries: allowing one to
interactively step through the legal concurrency behaviours,
pseudorandomly find legal outcomes, and exhaustively enumerate all
architecturally allowed outcomes of small bounded concurrent programs.
For ARM, it supports both the current ARMv8-A multicopy atomic model
and the earlier ARMv8-A non-multicopy-atomic model.

The tool provides both a stand-alone web-interface version (compiled to JavaScript), which can be tried at 
[http://www.cl.cam.ac.uk/users/pes20/rmem](http://www.cl.cam.ac.uk/users/pes20/rmem), and a command-line version.

rmem was developed as part of the
[REMS project](https://www.cl.cam.ac.uk/~pes20/rems/), partly funded
by ERC Advanced grant "ELVER"
(2018-2023, ERC AdG 789108), the EPSRC Programme Grant "REMS: Rigorous Engineering for
Mainstream Systems" (EP/K008528/1), EPSRC grant "C3: Scalable & Verified
Shared Memory via Consistency-directed Cache Coherence"
(EP/M027317/1), an ARM iCASE award (Pulte), an EPSRC IAA KTF (Gray),
EPSRC Leadership Fellowship "Semantic Foundations for Real-World
Systems" (EP/H005633/1, 2009-2014, Sewell), and EPSRC grant "Reasoning
with Relaxed Memory Models" (EP/F036345, 2008-2012).



## Dependencies

rmem relies on a number of dependencies, including:
- The concurrency models are defined in the [Lem specification
  language](https://github.com/rems-project/lem/).
- The instruction semantics are written in the [Sail ISA description
  language](https://github.com/rems-project/sail) (rmem currently
  temporarily depends on both versions 1 and 2).
- The instruction semantics also depend on the [Ott language specification
  tool](https://github.com/ott-lang/ott).
- For reading ELF binaries and their DWARF debug information, rmem uses the [Linksem model](https://github.com/rems-project/linksem).

The dependencies can be installed automatically as part of the rmem build process, as detailed below.


In addition, a debian based Linux system also need the following packages:

``` shell
sudo apt install findutils libgmp-dev m4 perl pkg-config zlib1g-dev
```


## Building and running rmem with command-line interface, with opam 

``` shell
  # add the REMS opam repository containing some of rmem's dependencies:
opam repository add rems https://github.com/rems-project/opam-repository.git#opam2
  # The ulimit is to work around the problem of the OCaml compiler running
  # out of memory when processing the ISA semantics.
ulimit -s 33000
opam install rmem
```


## Building and running rmem with command-line interface, with opam, from a github checkout

``` shell
  # clone the repo:
git clone git@github.com:rems-project/rmem.git
cd rmem
  # add the REMS opam repository containing some of rmem's dependencies:
opam repository add rems https://github.com/rems-project/opam-repository.git#opam2
  # The ulimit is to work around the problem of the OCaml compiler running
  # out of memory when processing the ISA semantics.
ulimit -s 33000
opam install .
```

To rebuild and reinstall after local changes, run `opam upgrade --working-dir rmem`  (or `opam upgrade -w rmem`).


## Building and running rmem with command-line interface, with `make`

Alternatively, rmem can be built using `make` as follows:

``` shell
  # clone the repo:
git clone git@github.com:rems-project/rmem.git
cd rmem
  # get rmem's opam dependencies
opam repository add rems https://github.com/rems-project/opam-repository.git
opam install --deps-only .
  # build rmem
ulimit -s 33000
make MODE=opt 
```

This will build a native rmem OCaml binary.
The invocation `make MODE=debug` builds an OCaml bytecode
version. To build rmem for only a specific ISA, for instance
AArch64, run `make rmem MODE=opt ISA=AArch64`. For additional
information and build options see `make help`.

Finally run `rmem --help` for information on how to run rmem and the
available options.


## Building rmem with web interface

TODO.

(The web interface is built using [JS_of_ocaml](https://ocsigen.org/js_of_ocaml/).)

## Authors

The contributors to the rmem code are listed in the LICENCE.txt file

## The concurrency models and their contributors

rmem is a successor to the ppcmem tool, originally developed to model
the concurrency semantics of the IBM Power architecture in the
"PLDI11" model, developed from 2009 onwards.  It has since been
substantially extended and re-engineered, and it currently includes
several concurrency models:

- The Flat model for ARMv8 and RISC-V, principally by Christopher
  Pulte and Shaked Flur, described in the paper and RISC-V architecture
  manual below, and co-developed with the revised ARMv8-A axiomatic
  model, principally by Will Deacon.

	- [Simplifying ARM Concurrency: Multicopy-atomic Axiomatic and
      Operational Models for
      ARMv8.](https://www.cl.cam.ac.uk/~pes20/armv8-mca/) Christopher
      Pulte, Shaked Flur, Will Deacon, Jon French, Susmit Sarkar, and
      Peter Sewell. In POPL 2018.
        - [The RISC-V Instruction Set Manual Volume I: Unprivileged ISA.](https://github.com/riscv/riscv-isa-manual/releases/download/draft-20181227-c6741cb/riscv-spec.pdf) Andrew Waterman and Krste Asanović, editors. December 2018. Document Version 20181221-Public-Review-draft. Contributors: Arvind, Krste Asanović, Rimas Avižienis, Jacob Bachmeyer, Christopher F. Batten, Allen J. Baum, Alex Bradbury, Scott Beamer, Preston Briggs, Christopher Celio, Chuanhua Chang, David Chisnall, Paul Clayton, Palmer Dabbelt, Roger Espasa, Shaked Flur, Stefan Freudenberger, Jan Gray, Michael Hamburg, John Hauser, David Horner, Bruce Hoult, Alexandre Joannou, Olof Johansson, Ben Keller, Yunsup Lee, Paul Loewenstein, Daniel Lustig, Yatin Manerkar, Luc Maranget, Margaret Martonosi, Joseph Myers, Vijayanand Nagarajan, Rishiyur Nikhil, Jonas Oberhauser, Stefan O'Rear, Albert Ou, John Ousterhout, David Patterson, Christopher Pulte, Jose Renau, Colin Schmidt, Peter Sewell, Susmit Sarkar, Michael Taylor, Wesley Terpstra, Matt Thomas, Tommy Thorn, Caroline Trippel, Ray VanDeWalker, Muralidaran Vijayaraghavan, Megan Wachs, Andrew Waterman, Robert Watson, Derek Williams, Andrew Wright, Reinoud Zandijk, and Sizhuo Zhang.

- The PLDI11 Power model, principally by Susmit Sarkar and Peter
  Sewell, originally as described in the papers below, and adapted since.

	- [Understanding POWER
      Multiprocessors.](https://www.cl.cam.ac.uk/~pes20/ppc-supplemental/index.html)
      Susmit Sarkar, Peter Sewell, Jade Alglave, Luc Maranget, and
      Derek Williams. In PLDI 2011.
	- [Clarifying and Compiling C/C++ Concurrency: from C++11 to
      POWER.](https://www.cl.cam.ac.uk/~pes20/cppppc/) Mark Batty,
      Kayvan Memarian, Scott Owens, Susmit Sarkar, and Peter
      Sewell. In POPL 2012.
	- [Synchronising C/C++ and
      POWER.](https://www.cl.cam.ac.uk/~pes20/cppppc-supplemental/)
      Susmit Sarkar, Kayvan Memarian, Scott Owens, Mark Batty, Peter
      Sewell, Luc Maranget, Jade Alglave, and Derek Williams. In PLDI 2012.

- The Flowing and POP models for ARMv8, principally by Shaked Flur,
  described in:
	- [Modelling the ARMv8 architecture, operationally: concurrency
      and ISA.](https://www.cl.cam.ac.uk/~sf502/popl16/index.html)
      Shaked Flur, Kathryn E. Gray, Christopher Pulte, Susmit Sarkar,
      Ali Sezgin, Luc Maranget, Will Deacon, and Peter Sewell. In POPL 2016.

- Originally all these models and the tool assumed all memory accesses were aligned and of the same
  size. Mixed-size support was added by Shaked Flur, Susmit Sarkar,
  Peter Sewell, and Kathryn E. Gray, as described in the paper:

  - [Mixed-size Concurrency: ARM, POWER, C/C++11, and SC.](https://www.cl.cam.ac.uk/~pes20/popl17/) Shaked Flur, Susmit
      Sarkar, Christopher Pulte, Kyndylan Nienhuis, Luc Maranget, Kathryn
      E. Gray, Ali Sezgin, Mark Batty, and Peter Sewell.

- The TSO model for x86, by Linden Ralph and Shaked Flur, adding mixed-size accesses and Sail ISA integration to the x86-TSO model described in:

	- [A better x86 memory model: x86-TSO](http://www.cl.cam.ac.uk/~pes20/weakmemory/x86tso-paper.tphols.pdf). Scott Owens, Susmit Sarkar, and Peter Sewell. In TPHOLs 2009. 

- The Promising model for ARMv8 and RISC-V, executable model
  principally by Christopher Pulte:
	- [Promising-ARM/RISC-V: a simpler and faster operational
      concurrency model.](https://sf.snu.ac.kr/promising-arm-riscv/)
      Christopher Pulte, Jean Pichon-Pharabod, Jeehoon Kang, Sung-Hwan
      Lee, and Chung-Kil Hur. In PLDI 2019.

### Model validation

These models have been validated against observable hardware behaviour using the litmus tool of the [herdtools](https://github.com/herd/herdtools7) tool suite of Jade Alglave and Luc Maranget; this validation has been done principally by Luc Maranget and Shaked Flur.    Some models have also been compared or co-developed with axiomatic models expressed using the herd tool of that tool suite.


### Sail integration

rmem integrates Sail models for the sequential aspects of the
instruction semantics.  The initial work, doing this for IBM POWER, is described in:

- [An integrated concurrency and core-ISA architectural envelope
  definition, and test oracle, for IBM POWER
  multiprocessors.](https://www.cl.cam.ac.uk/~pes20/micro-48-2015.pdf)
  Kathryn E. Gray, Gabriel Kerneis, Dominic P. Mulligan, Christopher
  Pulte, Susmit Sarkar, and Peter Sewell. In MICRO 2015.

More recent work, integrating a newer version of Sail with rmem (currently only for RISC-V), is described in:

- [ISA Semantics for ARMv8-A, RISC-V, and CHERI-MIPS.](http://www.cl.cam.ac.uk/users/pes20/sail/sail-popl2019.pdf) Alasdair Armstrong, Thomas Bauereiss, Brian Campbell, Alastair Reid, Kathryn E. Gray, Robert M. Norton, Prashanth Mundkur, Mark Wassell, Jon French, Christopher Pulte, Shaked Flur, Ian Stark, Neel Krishnaswami, and Peter Sewell. In POPL 2019.






### rmem infrastructure

rmem also includes extensive infrastructure for driving the models and providing user interfaces. 

- OCaml harness for driving the models, in exhaustive, pseudorandom,
  and interactive mode, infrastructure for connecting to the ELF
  model, pretty-printing the user interface, etc.: Shaked Flur, Susmit
  Sarkar, Jon French, Peter Sewell, Christopher Pulte, Luc Maranget,
  Pankaj Pawan, and Francesco Zappa Nardelli.

- The current graphical Web interface is principally by Jon French; an earlier version was principally by Pankaj Pawan and Francesco Zappa Nardelli.

- rmem integrates code from [herdtools](https://github.com/herd/herdtools7), in (src_top/herd_based), for parsing and pretty printing of litmus tests, maintaining compatibility with the herdtools tools. This code is due to Jade Alglave and Luc Maranget.

- Integrating dwarf information for improved user-interface: Peter
  Sewell and Shaked Flur.



## Directory structure


```
rmem
|
|---- pldi11, pldi11Opt, flat, etc. // shell scripts that run the corresponding 
|                                   // model in a suitable configuration. The 
|                                   // 'opt' version turns on the recommended 
|                                   // performance optimisations for exhaustive 
|                                   // execution
|
+---- src_top // OCaml code of the rmem tool
|     |
|     +---- herd_based // parsing litmus files (forked from herdtools)
|     +---- text       // CLI
|     +---- web        // web-interface
|     +---- headless   // CLI without interactive mode (does not depend on
|                      // lambda-term which does not build on Power)
|
+---- src_concurrency_model // Lem code of the concurrency models
|
+---- src_isa_defs          // Lem code defining details of the ISA models
|
+---- src_isa_stubs/<ISA>   // stubs for when the ISA is excluded from the build
|
+---- src_web_interface     // non-OCaml code of the web-interface (and some 
|                           // tests)
|
|
+---- gen_files // hand-written files helping with parsing/pretty-printing/etc.
|
+---- scripts // scripts for various things
|
|
+---- src_sail_legacy // temporary: a copy of the relevant bits of an old 
|                     // version of Sail and surrounding infrastructure: for 
|                     // models not ported to current Sail yet
|
| // THE FOLDERS BELOW ARE CONSTRUCTED DURING THE BUILD PROCESS AND ARE 
| // NOT CHECKED IN
|
+---- _build                 // ocamlbuild workspace
|
+---- generated_ocaml        // OCaml files generated from the concurrency and 
                             // ISA model Lem definitions

```

