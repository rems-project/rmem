General
=======

The web interface uses the Knockout (https://knockoutjs.com/)
framework for UI synchronisation with a data model, as well as
templates for parts of the UI. 


model.js
--------

- Defines a class StateModel and its singleton instance STATE;
- Registers all the UI component templates with Knockout (including a
custom one to load the 'help' from a separate URL);
- Applies data bindings to anything which has or descends from
  something with a 'ko' class.

graph.js
--------

- Sets up a web worker (running worker.js)  which actually runs
  graphviz in the background, to avoid slowing down the interface.
- Defines functions to display the graph and to set up the clickable
  links and hack around a font problem.
- Manages calls to and callbacks from the web worker.
- Sets up event handlers for pan/zoom of the graph on click of the
  buttons or mousewheel.
- Sets up event handlers so that graph is redrawn if engine setting changes.

isa.js
------

Provides the function 'read_filename' which is called from ocaml to
read in the interpreter defs files at runtime

layout.js
---------

Manages the splitter layout system, using Split.js

loaders.js
----------

Manages the loading of litmus and ELF files.

main.js
-------

Contains the 'entry point' to the app. Loads the ocaml, sets up the
default UI split or loads one from the URL hash, and contains
functions which are called by the UI. Re-does the setup if the URL
hash changes. Also manages the prompt, printing, and other things.

options.js
----------

Contains a mini framework for managing the option dropdown
menus. `make_toggler`, etc, add an option of the given type to the
given menu. See lines 420 onwards for usage. 

source.js
---------

Manages the 'source' view, using CodeMirror.

ui.js
-----

Contains event handlers for all the clickable etc UI elements such as
buttons, handling of keypresses, etc. Among other things, contains the
logic for creating 'link to this state' links.

utils.js
--------

Various utilities:
- Get/set/delete cookies
- Various modal dialogues e.g. error, message, processing
- Escaping
- Conversion of a jQuery XHR response to an exception or promise

worker.js
---------

Web worker for graph drawing, very simple wrapper that calls out to
Viz.js whenever it receives a message.

zarith.js
---------

Contains a partial reimplementation of the Zarith library's primitives
in Javascript, to avoid having to use Num. Used by `js_of_ocaml` when compiling.
