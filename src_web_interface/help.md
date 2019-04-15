% [RMEM](./)

RMEM is a tool for exploring the relaxed-memory concurrency behaviour allowed by
the ARM, IBM POWER, and RISC-V architectures; it also has experimental support for a mixed-size version of x86-TSO.

It lets one take small test programs, either concurrency litmus tests or
standalone ELF binaries, and explore the envelope of all their architecturally
allowed behaviour -- interactively, randomly, or exhaustively.

There is a web version for convenient use, compiled to JavaScript and running in
the browser (best used in chrome/chromium), and a command-line version (not
included here) for better performance and batch use.

# Getting Started

To use the web version:

  * Load a test to run, either a litmus test (from the built-in library, your
    local filesystem, or pasted in), or a small standalone ELF binary (from the
    built-in library or your local filesystem).

    Good tests to start with are the ARM or POWER `MP` test (in the litmus-test
    library Tutorial directory), that illustrates observable out-of-order writes
    and reads to distinct addresses, and then the ARM `MP+dmb.sy+ctrl` or POWER
    `MP+sync+ctrl`, that illustrate observable speculative reads.

  * Select your preferred model and model options.  RMEM supports several
    models:

    - for the revised MCA ARM v8 architecture, the Flat "other"
      multi-copy-atomic model
    - for the earlier non-MCA ARMv8 architecture, the Flowing and POP models
      described in the POPL 16 and POPL 17 papers, and the more abstract NOP
      model; and
    - for IBM POWER, the model described in the PLDI 11, PLDI 12, MICRO 15, and
      POPL 17 papers.
    - for RISC-V, the operational RVWMO model described in the RISC-V architecture specification.

    These are all operational models, defining model states and transitions.

  * The tool shows the initial model state, both in a textual console form and
    in a graphical form, and (underlined in each) the enabled transitions.  One
    can interactively select transitions by clicking on them, by typing their
    number into the console input box, or, for the default selected transition,
    by typing the Enter key.  The execution option "random" will make the
    default chosen randomly.

  * The model options let one execute intra-instruction semantics (the ASL or
    IBM pseudocode for each instruction, translated into Sail) either using the
    Sail interpreter, stepping through the pseudocode, or using the shallow
    embedding, in which the pseudocode is translated into a more direct
    representation of its behaviour.  For the former, one can choose whether to
    make the pseudocode execution visible, with the interface option to show
    Sail state.

  * There are further execution options and commands to control which
    transitions are taken (to take some kinds of transition eagerly,
    to focus on a single thread or
    instruction, and to insert breakpoints and watchpoints), and display and
    graph options to control what is output.  For interactive exploration, one
    usually wants to make some transition kinds eager.

  * The tool also supports search, of complete random executions and exhaustive
    search (the latter may be performance-limited in the web version).

  * Many things can be done either with menus or by typing commands into the
    console.  [The full console command help is below.](#full-console-help)

For ARM, POWER, and RISC-V, the models include essentially all the integer
non-vector user-mode instructions, with instruction semantics based on or
derived from the vendor pseudocode.  They include mixed-size behaviour; ARM
release/acquires and exclusives, `isb`, `dmb sy`, `dmb ld`, and `dmb st`; and
POWER load-reserve/store-conditional, `sync`, `lwsync`, and `isync`.  They do
not cover load-store pair/multiple, read-modify-writes, exceptions, interrupts,
floating-point, vector instructions, MMU aspects, or self-modifying code.

## Bugs and Workarounds

* If you have trouble with "stack overflow" errors when running searches in the
    web interface, try using Chrom{e,ium} with the command line option
    `--js-flags="--harmony-tailcalls"`, which enables experimental tail call
    optimisation.


# Papers

* [ISA Semantics for ARMv8-A, RISC-V, and CHERI-MIPS](http://www.cl.cam.ac.uk/users/pes20/sail/sail-popl2019.pdf). Alasdair Armstrong, Thomas Bauereiss, Brian Campbell, Alastair Reid, Kathryn E. Gray, Robert M. Norton, Prashanth Mundkur, Mark Wassell, Jon French, Christopher Pulte, Shaked Flur, Ian Stark, Neel Krishnaswami, and Peter Sewell. In POPL 2019, Proc. ACM Program. Lang. 3, POPL, Article 71

* [Simplifying ARM Concurrency: Multicopy-atomic Axiomatic and Operational Models for ARMv8](http://www.cl.cam.ac.uk/~pes20/armv8-mca/armv8-mca-draft.pdf).
  Christopher\ Pulte, Shaked\ Flur, Will\ Deacon, Jon\ French, Susmit\ Sarkar,
  Peter\ Sewell. In POPL 2018. [Further details](http://www.cl.cam.ac.uk/~pes20/armv8-mca/).

* [The Sail instruction-set semantics specification language](http://www.cl.cam.ac.uk/~pes20/sail/manual.pdf).
  Kathryn\ E.\ Gray, Peter\ Sewell, Christopher\ Pulte, Shaked\ Flur, Robert\ Norton-Wright. March\ 2017.

* [Mixed-size concurrency: ARM, POWER, C/C++11, and SC](http://www.cl.cam.ac.uk/users/pes20/popl17/mixed-size.pdf).
  Shaked\ Flur, Susmit\ Sarkar, Christopher\ Pulte, Kyndylan\ Nienhuis, Luc\ Maranget, Kathryn\ E.\ Gray, Ali\ Sezgin, Mark\ Batty, and Peter\ Sewell. In
  [POPL 2017](http://conf.researchr.org/home/POPL-2017). [(more details)](http://www.cl.cam.ac.uk/users/pes20/popl17/)

* [The missing link: explaining ELF static linking, semantically](http://www.cl.cam.ac.uk/~pes20/rems/papers/oopsla-elf-linking-2016.pdf).
  Stephen\ Kell, Dominic\ P.\ Mulligan, and Peter\ Sewell.
  In [OOPSLA 2016](http://2016.splashcon.org/track/splash-2016-oopsla).

* [Modelling the ARMv8 Architecture, Operationally: Concurrency and ISA](http://www.cl.cam.ac.uk/users/pes20/popl16-armv8/top.pdf).
  Shaked\ Flur, Kathryn\ E.\ Gray, Christopher\ Pulte, Susmit\ Sarkar, Ali\ Sezgin,
  Luc\ Maranget, Will\ Deacon, Peter\ Sewell.
  In
  [POPL 2016](http://conf.researchr.org/home/POPL-2016). [(more details)](http://www.cl.cam.ac.uk/users/pes20/popl16-armv8/).

* [An integrated concurrency and core-ISA architectural envelope definition, and test oracle, for IBM POWER multiprocessors](http://www.cl.cam.ac.uk/users/pes20/micro-48-2015.pdf).
  Kathryn\ E.\ Gray, Gabriel\ Kerneis, Dominic\ Mulligan, Christopher\ Pulte,
  Susmit\ Sarkar, Peter\ Sewell.
  In [MICRO-48](http://www.microarch.org/micro48/),\ 2015.

* [A Tutorial Introduction to the ARM and POWER Relaxed Memory Models](http://www.cl.cam.ac.uk/users/pes20/ppc-supplemental/test7.pdf).
  Luc\ Maranget, Susmit\ Sarkar, and Peter\ Sewell. Draft.

* [Synchronising C/C++ and POWER](http://www.cl.cam.ac.uk/~pes20/cppppc-supplemental/pldi010-sarkar.pdf).
  Susmit\ Sarkar, Kayvan\ Memarian, Scott\ Owens, Mark\ Batty, Peter\ Sewell, Luc\ Maranget, Jade\ Alglave, Derek\ Williams. In
  [PLDI 2012](http://pldi12.cs.purdue.edu/). [(more details)](http://www.cl.cam.ac.uk/~pes20/cppppc-supplemental/)

* [Understanding POWER Multiprocessors](http://www.cl.cam.ac.uk/users/pes20/ppc-supplemental/pldi105-sarkar.pdf).
  Susmit\ Sarkar, Peter\ Sewell, Jade\ Alglave, Luc\ Maranget, Derek\ Williams. In
  [PLDI 2011](http://pldi11.cs.utah.edu/). [(more details)](http://www.cl.cam.ac.uk/users/pes20/ppc-supplemental/index.html)

# People

* The concurrency models are by
  [Shaked\ Flur](http://www.cl.cam.ac.uk/~sf502) (Cambridge),
  [Christopher\ Pulte](http://www.cl.cam.ac.uk/~cp526/) (Cambridge),
  [Susmit\ Sarkar](http://www.cl.cam.ac.uk/~ss726/) (St.\ Andrews),
  and
  [Peter\ Sewell](http://www.cl.cam.ac.uk/~pes20) (Cambridge),
 with contributions from Linden Ralph (Cambridge) for the x86 model.
  They build on extensive discussion with
  Will\ Deacon (ARM),
  Richard\ Grisenthwaite (ARM),
  Derek\ Williams (IBM),
  and on experimental testing done jointly with
  [Luc\ Maranget](http://pauillac.inria.fr/~maranget/) (INRIA\ Paris).

* The instruction semantics are expressed in the
  [Sail](http://www.cl.cam.ac.uk/~pes20/sail/)
  language, principally by
  [Kathryn\ Gray](http://www.cl.cam.ac.uk/~keg29/) (ex-Cambridge),
  and with shallow embedding work by
  [Christopher\ Pulte](http://www.cl.cam.ac.uk/~cp526/).

* The ARM and POWER ISA models are by
  [Shaked\ Flur](http://www.cl.cam.ac.uk/~sf502),
  [Christopher\ Pulte](http://www.cl.cam.ac.uk/~cp526/),
  [Kathryn\ Gray](http://www.cl.cam.ac.uk/~keg29/),
  and
  [Gabriel\ Kerneis](http://gabriel.kerneis.info/) (ex-Cambridge).
  The x86 ISA model is by
  [Robert Norton-Wright](http://www.cl.cam.ac.uk/~rmn30) (Cambridge) and
  [Anthony Fox](http://www.cl.cam.ac.uk/~acjf3) (Cambridge).
  The RISC-V ISA model is by
  [Robert Norton-Wright](http://www.cl.cam.ac.uk/~rmn30) (Cambridge).

* The web interface and search control are by
  [Jon\ French](http://www.cl.cam.ac.uk/~jf451) (Cambridge).

* The ELF front end uses the formal specification of ELF and parts of DWARF from the
  [linksem](https://bitbucket.org/Peter_Sewell/linksem)
  project, by
  [Dominic\ Mulligan](http://dominic-mulligan.co.uk/) (Cambridge),
  [Stephen\ Kell](http://www.cl.cam.ac.uk/~srk31/) (Cambridge),
  and
  [Peter\ Sewell](http://www.cl.cam.ac.uk/~pes20).

* The litmus front end uses code from the
  [diy](http://diy.inria.fr/)
  suite, which is by
  [Jade\ Alglave](http://www0.cs.ucl.ac.uk/staff/J.Alglave/) (UCL and MSRC)
  and
  [Luc\ Maranget](http://pauillac.inria.fr/~maranget/).

* The model is expressed using [Lem](http://www.cl.cam.ac.uk/~pes20/lem/), which
  is compiled to OCaml and (for the web interface) thence to JavaScript,
  using [js_of_ocaml](http://ocsigen.org/js_of_ocaml/manual/).


RMEM also builds on earlier work on tool and model development, for the
`ppcmem2`, `ppcmem`, and `memevents` tools, including contributions from
[Jade\ Alglave](http://www0.cs.ucl.ac.uk/staff/J.Alglave/),
[Ohad\ Kammar](http://www.cs.ox.ac.uk/people/ohad.kammar/main.html) (ex-Cambridge),
[Pankaj\ Pawan](http://iitk.academia.edu/PankajPawan) (ex-IIT Kanpur and INRIA\ Paris),
[Ali\ Sezgin](http://www.cl.cam.ac.uk/~as2418/) (ex-Cambridge),
and
[Francesco\ Zappa\ Nardelli](http://moscova.inria.fr/~zappa) (INRIA\ Paris).


# Disclaimer

Note that, while substantial confidence in the models has been established, by
experiment, by discussion with the vendors, and (for the ARM Flat model) by an
equivalence proof with the revised ARM architecture axiomatic model, they are
not the vendor architecture documents of either IBM or ARM.

# Funding

This work is funded by
[REMS: Rigorous Engineering for Mainstream Systems](http://www.cl.cam.ac.uk/users/pes20/rems),
by an EPSRC iCASE studentship, and by an EPSRC Impact Acceleration Account
Knowledge Transfer Fellowship (with ARM).

# User Guide

## Tests

The menu-bar **Load litmus test** and **Load ELF test** buttons
let one choose a test to run, either a litmus test or a standalone ELF
binary.

One can choose tests from the built-in libraries, which include many
tests mentioned in the literature, select files from the local
filesystem, or paste tests into an editing window.

The litmus library is subdivided into categories, shown on the left.
If one starts to type a test name in the "Filter" box, it shows the
categories and matching tests within the currently selected category.

Litmus tests are in essentially the same format as used by the litmus tool for
running tests on hardware (see
the [TACAS\ 2011 paper](https://www.cl.cam.ac.uk/~pes20/weakmemory/tacas11.pdf))
and the herd tool for axiomatic models, both part of
the [diy](http://diy.inria.fr/) suite, except that the supported instructions
may differ.

Standalone ELF binaries can be produced (for example) by compiling C programs
with `gcc` or `clang`, e.g. using the `gcc` flags

```
CFLAGS=-g -std=c11 -Wall -save-temps=obj -ffreestanding -nostdlib -Wl,--defsym,_start=main -Wa,-L -D__THREAD_START_H=\"thread_start_aarch64.h\"
```

for ARM and

```
CFLAGS= -g -std=c11 -mcpu=power7 -Wall -save-temps -msoft-float -ffreestanding -nostdlib -Wl,--defsym,_start=main   -Wa,-L
```

for POWER.

The models support a special "thread start" pseudo-instruction to start a new
hardware thread; see the `atomics_test_2.c` example in the ELF test library for
example usage and the appropriate ARM or POWER header file. The maximum number
of threads must be configured in the "Load ELF test" dialogue.

## Model options

The menu-bar **Model Options** lets one control which concurrency model to use
(the available models and options differ for ARM and POWER tests).  The models
are:

* for the revised MCA ARM v8 architecture, the Flat "other" multi-copy-atomic model
* for the earlier non-MCA ARMv8 architecture, the Flowing and POP models
  described in the POPL 16 and POPL 17 papers, and the more abstract NOP model; and
* for IBM POWER, the model described in the PLDI 11, PLDI 12, MICRO 15, and POPL
  17 papers.

These are all operational models, defining model states and transitions.

For the intra-instruction semantics, one can choose either the Sail interpreter
(allowing single-stepping through the Sail pseudocode for each instruction
instance) or the "Shallow embedding" (computing the next outcome of each
instruction instance directly).

The *Out-of-order mode* lets one choose whether to allow simultaneous
exploration of multiple successors of conditional branches ("tree speculation"
allowed) or restrict to exploration of a single path at a time ("tree
speculation" forbidden).  This should not affect the set of final allowed
states.  For performance, one often wants to forbid tree speculation.

In the Flowing model, one has to choose the desired topology.

## Panes

The main UI consists of a menu bar and one or more panes.  Each pane
can display one of:

* **Console**: a text view of the current state of the model, together with a
  list of the transitions followed so far and a console to enter commands.
* **Graph**: a graphical view of the current state of the model.
* **Sources**: the source file(s) of the test currently being run.
* **Trace**: the trace of transitions executed so far.
* **Help**: this help text.  

Panes can be subdivided horizontally or vertically, or deleted, with
the buttons at their top right corner.  The boundaries between them
can be dragged.  The font size of each pane can be adjusted
separately.  The graph can be dragged within its pane.

## Interactive exploration

The console, graph, and trace panes each show the enabled transitions of the
current model state, underlined and highlighted in blue; one can just click on
these to construct a possible (architecture/model-allowed) execution
incrementally.  The **Undo** and **Restart** menu-bar buttons undo transitions
and restart the execution, respectively.

### Eager transitions

Depending on what one wants to explore, one may want to make one or more kinds
of transition "eager", using the **Execution** options drop-down in the
menu-bar.

For stepping through the pseudocode of a single instruction, one typically wants
nothing eager, while for exploring the allowed concurrent execution of a litmus
test, one might want all the below eager.

Making transitions eager means they will be automatically taken whenever
enabled, leaving only the non-eager transitions visible.  Several kinds of
transition can independently be made eager:

* Eager fetch (single-successor)
* Eager fetch (multiple-successor)
* Eager pseudocode-internal transitions
* Eager constant-register reads
* Eager register reads and writes
* Eager memory auxiliary transitions
* Eager instruction finish
* Eager footprint recalculation
* Eager thread start

One can also select all or none of those.  Taking any of these eagerly should
not eliminate any potential executions.  For the Flat model, the transitions for
satisfying a memory read and propagating a memory write cannot be made eager, as
in general taking one such transition may exclude some other behaviour.  For
code with loops, making instruction fetch eager will not terminate, otherwise it
can be eager. The console `fetch` command will also fetch all the instruction
instances.

In the web interface version, browsers without tail-call optimisation may hit
stack overflow issues.

### Default transitions

One of the enabled transitions is selected as the default, which can be taken
just be pressing Enter; the default transition is highlighted in yellow.  The
**Execution** drop-down Random option makes the default be chosen randomly,
otherwise it is normally chosen in a deterministic (but rather arbitrary) way.

### Follow-mode

The enabled transitions in each state are numbered, and the list of choices made
so far is displayed in the console window.  To replay a trace interactively, one
can set the "follow list" to such a list, with the console ` set follow_list `
command, which then makes the default transition that from the next step in the
follow list.

### Interface display options

The menu-bar **Interface** drop-down gives several options for controlling the
amount of detail in the console output. Here the main options are "Show Sail
state and code", controlling whether the detailed pseudocode state for each
instruction instance is displayed, and the "PP Style" which can be "compact" or
"full".

For larger tests, especially compiled ELF files, one often wants to suppress
most of the prefix of finished instructions, by setting the "Hide finished
instructions but last..." interface option.

### Graph display options

The menu-bar **Graph** drop-down gives several options for controlling the graph
output:

* Show registers, to show the input and output registers of each instruction
* Show register read-froms, show the register read-from edges
* Show transitions, to show the transitions of each instruction
* Only shared instructions, to suppress all instructions that do not access
  locations that (in this execution so far) have been shared

By default the graph is redrawn after every transition, but for large graphs
this may be too slow, in which case "Update every step" can be turned off and
one can use the manual "Refresh" button on the graph pane.

For searches, one can also choose whether to display a graph of a final state
that satisfies the condition of a litmus test, "Final OK graph".

The "Download .dot" button on the graph pane title lets one download the
GraphViz dot source for the displayed graph.

## Random search

The **Search** menu-bar drop-down includes a "Search randomly" button, which
explores some user-selectable number of random traces. For each trace, it start
with the current state and takes a sequence of pseudo-randomly chosen
transitions.  The results are shown as a histogram of the values in the
reachable final states, with (for each) a sample trace (expressed as a
follow-mode list of transition numbers) that reached those values.

These histograms are in the same format as those produced by the `litmus` tool,
to let one compare model results with experimental results from running tests on
hardware (usually using model results from running the console version on
batches of tests).

## Exhaustive search

The **Search** menu-bar drop-down also includes a "Search exhaustively" button,
which tries an exhaustive search from the current state.  To reduce the state
space, one usually wants:

* Search option "hash prune" on. This optimises by identifying states that have
  the same hash.
* Search option "prune restarts" on.  This optimises by considering only
  executions with no restarts.
* All the Execution eager options on, except in forbid-tree-speculation mode,
  where eager fetch (multiple successor) would be unsound.

Exhaustive search also prints a histogram of the reachable final states on the
console.

For both random-trace and exhaustive search, one can set limits, of the amount
of time or number of transitions.

## Stepping control

The console interface provides additional control of stepping:

* `step N` takes `N` default transitions
* `back N` undoes `N` transitions
* `auto` repeatedly takes the default transition until there are no more
* `follow` repeatedly takes the next follow-mode transition until there are no
   more (or the stated transition number does not exist)
* `focus thread (N|off)` enables only transitions of Thread `N` (or turns that off)
* `focus instruction (<ioid>|off)` enables only transitions of instruction  `ioid` (or turns that off)
* `stepi N1:N2 ` Steps one instruction.  Specifically, it searches for and takes
   the shortest path of transitions to a state in which instruction `N2` of
   thread `N1` is finished
* `set random <bool> ` turns random choice of the default transition on or off

## ELF/DWARF Debug information output

When running an ELF file that was compiled with debugging information, RMEM will
by default use that information in the output: using the source-code names for
global and local variables, and showing the C source code lines associated with
assembly instructions.  This can be toggled with "Enable DWARF support" in the
"Load ELF test" dialogue.  The tool does not currently use DWARF type
information, e.g. for displaying values of C struct types.

## Debugger-style breakpoints and watchpoints

For larger tests (especially ELF files compiled from non-trivial C code), one
may want to automatically execute up to some condition, in the same way as one
might use a debugger such as `gdb`, rather than single-step.

The console `break` and `watch` commands let one set breakpoints at particular
addresses (numeric or a symbol with a numeric offset) and for particular source
lines, and watchpoints for particular addresses (numeric or a symbol with a
numeric offset), of a specified size (in bytes).  Watchpoints can be for reads,
writes, or both.  These apply to exhaustive and random search.

Along with this, one sometimes wants to turn off the automatic redisplay of the
model state in the console, with the **Interface** dropdown "Always print"
option.  One can also toggle "Scroll on output" and "Suppress newpage".

## Linking to model/tool states

When discussing some particular example, it is often useful to communicate
particular executions.  This can be done in two ways: one can either just
manually paste the follow-mode list of transition numbers (note that this will
only be valid for the same eager-mode options), but one can also use the
menu-bar **Link to this state** dropdown to construct a URL that includes that,
and optionally also includes other information.

# Full console help {#full-console-help}

<!-- Local Variables:                -->
<!-- mode:        markdown           -->
<!-- fill-column: 80                 -->
<!-- eval:        (auto-fill-mode 1) -->
<!-- End:                            -->
