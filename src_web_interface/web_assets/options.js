/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Jon French, University of Cambridge  2017-2018                     */
/*  Copyright Shaked Flur, University of Cambridge 2017-2018                     */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

/* -*- eval: (electric-indent-mode 0) -*- */
/* the above is due to alignment of option specs in this file */

var model_options_dialog = $("#options").dialog({
        autoOpen: false,
        modal: true,
        width: 500,
        buttons: {
            "Cancel": function () {
                $(this).dialog("close");
            },
            "Defaults": function () {
                webppc_lib.resetOptions();
            },
            "OK": function () {
                $(this).dialog("close");
                if (started) {
                    restart();
                }
            }
        },
        close: clear_this_errors
    });


function show_hide_select_topology () {
    if ($("#model_flowing").is(":checked")) {
        $("#flowing_topology").show();
        $("#flowing_topology_na").hide();
    } else {
        $("#flowing_topology").hide();
        $("#flowing_topology_na").show();
    }
}

var options_updated = false;
var options = {}

function make_toggler (group, option_id, name, desc, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "toggler",
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="top"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + "></div>");
            $('<input type="checkbox" id="' + opt.id + '-toggle"'
              + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
              + '>')
                .on("change", function (e) {
                    var cmd = "set " + opt.id + " " + ($(this).prop("checked") ? "on" : "off");
                    do_command(cmd);
                }).appendTo(div);
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);
            return div;
        },
        update_func: function (opt, value) {
            var v = !!value;
            $("#" + opt.id + "-toggle").prop("checked", v);
        },
        data_bind: data_bind
    };
}

function make_custom_toggler (group, option_id, name, desc, values, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "toggler",
        disabled_msg: disabled_msg,
        values: values,
        create_func: function (opt) {
            var div = $('<div class="top"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + "></div>");
            $('<input type="checkbox" id="' + opt.id + '-toggle"'
              + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
              + '>')
                .on("change", function (e) {
                    var cmd = "set " + opt.id + " " + ($(this).prop("checked") ? opt.values[0] : opt.values[1]);
                    do_command(cmd);
                }).appendTo(div);
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);
            return div;
        },
        update_func: function (opt, value) {
            var v = !!value;
            $("#" + opt.id + "-toggle").prop("checked", value === opt.values[0]);
        },
        data_bind: data_bind
    };
}


function make_virtual_toggler (group, option_id, name, desc, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "toggler",
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="top"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + "></div>");
            $('<input type="checkbox" id="' + opt.id + '-toggle"'
              + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
              + '>')
                .appendTo(div);
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);
            return div;
        },
        update_func: function () { },
        data_bind: data_bind
    };
}

function make_dropdown (group, option_id, name, desc, choices, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "dropdown",
        choices: choices,
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="top"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + "></div>");
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);

            var select = $('<select id="' + opt.id + '-select"></select>');
            for (var key in opt.choices) {
                $('<option value="' + key + '">' + opt.choices[key] + "</option>")
                    .appendTo(select);
            }
            select.appendTo(div);
            select.on("change", function (e) {
                var cmd = "set " + opt.id + " " + select.val();
                do_command(cmd);
            });

            return div;
        },
        update_func: function (opt, value) {
            var dropdown = $("#" + opt.id + "-select");
            dropdown.val(value);
        },
        data_bind: data_bind
    };
}

function make_model_dropdown (group, option_id, name, desc, choices, accessor, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "dropdown",
        choices: choices,
        accessor: accessor,
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="top"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + "></div>");
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);

            var select = $('<select id="' + opt.id + '-select"></select>');
            var current = opt.accessor();
            for (var key in opt.choices) {
                $('<option value="' + key + '"'
                  + ((current === key) ? ' selected="selected"' : '')
                  + '>' + opt.choices[key] + "</option>")
                    .appendTo(select);
            }
            select.appendTo(div);
            select.on("change", function (e) {
                opt.accessor(select.val());
            });
            opt.accessor.subscribe(function (value) {
                select.val(value);
            });

            return div;
        },
        update_func: function () { },
        data_bind: data_bind
    };
}

function make_int_option (group, option_id, name, desc, data_bind, disabled_msg, suffix) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "int_option",
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="int_option_control top" id="' + opt.id + '-div"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + '></div>');
            $('<input type="checkbox" id="' + opt.id + '-toggle"'
              + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
              + '>')
                .on("change", function (e) {
                    var cmd;
                    if (!$(this).prop("checked")) {
                        cmd = "set " + opt.id + " none";
                    } else {
                        cmd = "set " + opt.id + " " + $("#" + opt.id + "-button").text();
                    }
                    do_command(cmd);
                }).appendTo(div);
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);
            $('<button id="' + opt.id + '-button">0</button>')
                .click(function () {
                    var value = window.prompt("Enter new value for '" + opt.name + "'", $(this).text());
                    if (value != null) {
                        var cmd = "set " + opt.id + " " + value;
                        do_command(cmd);
                    }
                }).appendTo(div);
            if (suffix) {
                $('<span class="option-suffix">&nbsp;' + suffix + '</span>').appendTo(div);
            }
            return div;
        },
        update_func: function (opt, value) {
            var toggle_value;
            var button = $("#" + opt.id + "-button");
            if (value === null) {
                toggle_value = false;
            } else {
                toggle_value = true;
                button.text(value.toString());
            }
            $("#" + opt.id + "-toggle").prop("checked", toggle_value);
        },
        data_bind: data_bind
    };
}

function make_virtual_int (group, option_id, name, desc, data_bind, disabled_msg) {
    options[option_id] = {
        id: option_id,
        name: name,
        desc: desc,
        group: group,
        cls: "int",
        disabled_msg: disabled_msg,
        create_func: function (opt) {
            var div = $('<div class="int_control top" id="' + opt.id + '-div"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + '></div>');
            var label = $('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>");
            if (opt.disabled_msg) {
                $('<span class="disabled_msg" title="' + opt.desc + '">&nbsp;(' + opt.disabled_msg + ")</span>").appendTo(label);
            }
            label.appendTo(div);
            $('<button id="' + opt.id + '-button">1</button>')
                .click(function () {
                    var that = $(this);
                    var value = window.prompt("Enter new value for '" + opt.name + "'", that.text());
                    if (value != null && /^\d+$/.test(value)) {
                        that.text(value.toString());
                    } else {
                        window.alert("Invalid value, defaulting to 1.");
                        that.text("1");
                    }
                }).appendTo(div);
            return div;
        },
        update_func: function () { },
        data_bind: data_bind
    };
}

function make_button_row (group, option_id, buttons, data_bind) {
    options[option_id] = {
        id: option_id,
        group: group,
        cls: "button_row",
        data_bind: data_bind,
        create_func: function (opt) {
            var div = $('<div class="options_button_row top" id=' + opt.id + '-div"'
                        + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '')
                        + '></div>');
            for (var name in buttons) {
                $('<a href="#"></a>')
                    .text(name)
                    .click(buttons[name])
                    .appendTo(div);
            }
            return div;
        },
        update_func: function () { }
    };
}

function make_separator (group, option_id, name) {
    options[option_id] = {
        id: option_id,
        group: group,
        cls: "group_separator",
        name: name,
        create_func: function (opt) {
            return $('<span>' + name + '</span>');
        },
        update_func: function () { }
    };
}

function make_line (group, option_id) {
    options[option_id] = {
        id: option_id,
        group: group,
        cls: "line_separator",
        create_func: function (opt) {
            return $("<span>&nbsp;</span>");
        },
        update_func: function () { }
    };
}



//                    group                id                                       name                         desc
        make_toggler("Execution",         "random",                                "Random",                    "Choose the default transition pseudorandomly");
        make_toggler("Execution",         "suppress_internal",                     "Suppress pseudocode internal", "Automatically take internal Sail transitions", "css: { disabled: embedding() !== 'interpreter' }", "N/A to shallow embedding");
        make_toggler("Execution",         "storage_first",                         "Storage first",             "Take storage transitions preferentially when stepping", "css: { disabled: model_name() === 'flat' }", "N/A to flat model");
     make_int_option("Execution",         "loop_limit",                            "Loop unroll limit (EXPERIMENTAL)", "Limit loops to N repeats: i.e. limit = N -> (N+1) unrolled iterations through loop");
      make_separator("Execution",         "execution_eager",                       "Eager modes");

make_button_row("Execution", "eager_shortcut_buttons_all", {
    "All eager": function (e) {
        do_command("set eager on");
        e.preventDefault();
        return false;
    }
});
make_button_row("Execution", "eager_shortcut_buttons_none", {
    "None eager": function (e) {
        do_command("set eager off");
        e.preventDefault();
        return false;
    }
});
           make_line("Execution",         "eager_sep");
        make_toggler("Execution",         "eager_fetch_single",                    "Eager fetch (single-successor)", "Are single-successor fetches considered eagerly takeable?");
        make_toggler("Execution",         "eager_fetch_multi",                     "Eager fetch (multiple-successor)", "Are multiple-successor fetches (e.g. branches) considered eagerly takeable?");
        make_toggler("Execution",         "eager_pseudocode_internal",             "Eager pseudocode internal", "Are pseudocode-internal transitions considered eagerly takeable?", "css: { disabled: embedding() !== 'interpreter' }", "N/A to shallow embedding");
        make_toggler("Execution",         "eager_constant_reg_read",               "Eager constant reg reads",  "Are constant reg reads considered eagerly takeable?");
        make_toggler("Execution",         "eager_reg_rw",                          "Eager register read/write", "Are all register reads/writes considered eagerly takeable?");
        make_toggler("Execution",         "eager_memory_aux",                      "Eager memory aux",          "Are memory auxiliary transitions considered eagerly takeable?");
        make_toggler("Execution",         "eager_finish",                          "Eager finish",              "Are instruction finishes considered eagerly takeable?");
        make_toggler("Execution",         "eager_fp_recalc",                       "Eager footprint recalc",    "Are footprint calculations considered eagerly takeable?");
        make_toggler("Execution",         "eager_thread_start",                    "Eager thread start",        "Are thread starts considered eagerly takeable?");
        make_toggler("Execution",         "eager_local_mem",                       "Eager local memory",        "Are memory access to local memory transitions considered eagerly takeable?");

      make_separator("Execution",         "execution_advanced",                    "Advanced execution options");
        make_toggler("Execution",         "priority_reduction",                    "Priority reduction",        "Take priority transitions preferentially (currently: exclusives and multi-successor fetches)");

 make_custom_toggler("Interface",         "pp_style",                              "Show instruction metadata", "Show or hide instruction bookkeeping/metadata in state printing, e.g. register read/writes", ["full", "compact"]);
        make_toggler("Interface",         "pp_sail",                               "Show Sail state and code",  "Show the Sail interpreter state and code for each in-flight instruction. Note: " +
                                                                                                                "only works if the Sail interpreter is being used, as opposed to the shallow embedding.", "css: { disabled: embedding() !== 'interpreter' }", "N/A to shallow embedding")
        make_toggler("Interface",         "condense_finished_instructions",        "Condense finished instructions", "Condense finished instructions in states");
     make_int_option("Interface",         "max_finished",                          "Hide finished instructions but last", "Maximum number of finished instructions printed at the start of each thread");

      make_separator("Interface",         "interface_advanced",                    "Advanced interface options");

        make_toggler("Interface",         "always_print",                          "Print every step",          "Print the state before each prompt");
make_virtual_toggler("Interface",         "scroll_on_output",                      "Scroll on output",          "Scroll to the bottom when new output is printed");
        make_toggler("Interface",         "suppress_newpage",                      "Suppress newpage",          "Don't clear the screen before each prompt");
        make_toggler("Interface",         "buffer_messages",                       "Buffer messages",           "Hold messages until the next prompt is displayed");
        make_toggler("Interface",         "announce_options",                      "Announce options",          "Print the current options before each prompt");
        make_toggler("Interface",         "pp_colours",                            "Colour",                    "Print colours in output");
        make_toggler("Interface",         "pp_hex",                                "Hex",                       "Print hex instead of decimal in output");
        make_toggler("Interface",         "prefer_symbolic_values",                "Prefer symbolic values",    "Prefer printing symbolic values in states");
        make_toggler("Interface",         "hide_pseudoregister_reads",             "Hide pseudoregister reads", "Hide or show pseudoregister reads in output");
     make_int_option("Interface",         "choice_history_limit",                  "Choice history limit",      "Maximum number of 'choices so far' printed with states");
        make_toggler("Interface",         "dwarf_show_all_variable_locations",     "Show all DWARF var locations", "Show all DWARF variable location data in output");

        make_toggler("Graph",             "always_graph",                          "Update every step",         "Update the graph before each prompt");
        make_toggler("Graph",             "dot_final_ok",                          "Final OK graph",            "Update the graph for the first final-constraint-satisfying execution of searches");
        make_toggler("Graph",             "dot_final_not_ok",                      "Final not OK graph",        "Update the graph for the first final-constraint-not-satisfying execution of searches");
        make_toggler("Graph",             "ppg_shared",                            "Only show shared instructions", "Only graph shared-memory instructions");
        make_toggler("Graph",             "ppg_rf",                                "Show read-from (rf) edges", "Display read-from (rf) edges in the graph");
        make_toggler("Graph",             "ppg_fr",                                "Show from-read (fr) edges", "Display from-read (fr) edges in the graph. These indicate writes co-after the write which satisfied a read.");
        make_toggler("Graph",             "ppg_co",                                "Show coherence (co) edges", "Display coherence (co) edges in the graph");
        make_toggler("Graph",             "ppg_addr",                              "Show address dependency (addr) edges", "Display address dependency (addr) edges in the graph");
        make_toggler("Graph",             "ppg_data",                              "Show data dependency (data) edges", "Display data dependency (data) edges in the graph");
        make_toggler("Graph",             "ppg_ctrl",                              "Show control dependency (ctrl) edges", "Display control dependency (ctrl) edges in the graph");
        make_toggler("Graph",             "ppg_regs",                              "Show registers",            "Display registers in the graph");
        make_toggler("Graph",             "ppg_reg_rf",                            "Show register read-froms",  "Display register read-from edges in the graph");
        make_toggler("Graph",             "ppg_trans",                             "Show transitions",          "Display transitions in the graph");

     make_button_row("Search", "search_buttons_random", {
         "Random": function (e) {
             var n = $("#random_trials-button").text();
             // scroll into view even if scroll-on-output is off
             scroll_anyway = true;
             do_command("search random " + n);
             e.preventDefault();
             return false;
         }
     });
     make_button_row("Search", "search_buttons_exh", {
         "Exhaustive": function (e) {
             confirm_dialog("Warning: Exhaustive search is highly likely to stack overflow\n"
                            + "unless your browser has tail call optimisation. Continue?",
                            function () {
                                console.log("Exhaustive search started: " + new Date().toString());
                                // scroll into view even if scroll-on-output is off
                                scroll_anyway = true;
                                do_command("search exhaustive");
                                console.log("Exhaustive search finished: " + new Date().toString());
                            });
             e.preventDefault();
             return false;
         }
     });
      make_separator("Search",            "search_header",                         "Search options");
        make_toggler("Search",            "hash_prune",                            "Hash prune",                "Avoid visiting subtrees more than once by hashing states");
        make_toggler("Search",            "prune_restarts",                        "Prune restarts",            "Prune traces with instruction restarts");
        make_toggler("Search",            "prune_discards",                        "Prune discards",            "Prune traces with instruction discards (requires forbid_tree_speculation)", "css: { disabled: tree_speculation() !== 'forbid' }", "requires forbid_tree_speculation");
     make_int_option("Search",            "time_limit",                            "Time limit",                "Time limit on searches", undefined, undefined, "seconds");
      make_separator("Search",            "search_random_options",                 "Random search options");
    make_virtual_int("Search",            "random_trials",                         "Number of trials",          "The number of times to randomly search for a final state");
      make_separator("Search",            "search_advanced",                       "Advanced search options");
     make_int_option("Search",            "transition_limit",                      "Transition limit",          "Transition limit on searches");
     make_int_option("Search",            "trace_limit",                           "Trace limit",               "Trace limit on searches");
        make_toggler("Search",            "partial_order_reduction",               "Partial order reduction",   "Restrict searches according to the partial order reduction");
        make_toggler("Search",            "compare_analyses",                      "Compare analyses",          "Compare the handwritten and exhaustive analyses");


       make_dropdown("Interface",         "verbosity",                             "Verbosity",                 "How much detail to print", {
           "quiet": "Quiet",
           "normal": "Normal",
           "verbose": "Verbose",
           "very": "Very verbose",
           "debug": "Debug"
       });

make_model_dropdown("Graph",            "graph_position_engine",                 "Position engine",           "The graphviz engine to use for laying out positions of nodes in a first layout pass ('none' for 'no position pass')", {
    "(none)": "(none)",
       "dot": "dot",
     "neato": "neato",
     "circo": "circo"
}, STATE.graph_position_engine);

make_model_dropdown("Graph",            "graph_edge_engine",                     "Edge engine",               "The graphviz engine to use for laying out edges", {
       "dot": "dot",
     "neato": "neato",
     "circo": "circo"
}, STATE.graph_edge_engine);

var last_options = null;

function update_options (updates) {
    console.log("updating options:", updates);
    if (!options_updated) {
        options_updated = true;
    }
    last_options = updates;
    for (var option_id in options) {
        options[option_id].update_func(options[option_id], updates[option_id]);
    }
}

function set_all_options(options) {
    if (options !== null) {
        do_command("silence;" +
                   Object.keys(options).map(function (option_id) {
                       var value = options[option_id];
                       var option_str = (value === null ? "none" : value.toString());
                       return "set " + option_id + " " + option_str;
                   }).join(";"));
    }
}

$(document).ready(function () {
    $("#select_options").click(function(e) {
        model_options_dialog.dialog("open");
    });

    $("#options input[type=radio][name=model]").on("change", function() {
        // only the checked radio is triggered
        show_hide_select_topology();
    });
});

var option_groups = {}

for (var option_id in options) {
    var opt = options[option_id];
    var div = $('<div class="option ' + opt.cls + '"' + (opt.data_bind ? ' data-bind="' + opt.data_bind + '"' : '') + '></div>');
    //var topDiv = $('<div class="top"></div>');
    //var bottomDiv = $('<div class="bottom"></div>');
    //$('<span class="option-name" title="' + opt.desc + '">' + opt.name + "</span>").appendTo(topDiv);
    //$('<span class="option-control" title="' + opt.desc + '"></span>').append(opt.create_func()).appendTo(topDiv);

    // Descriptions disabled for now as they take a lot of space.
    // They can still be read in tooltip form.

    // $('<span class="option-desc">' + opt.desc + "</span>").appendTo(bottomDiv);
    $(opt.create_func(opt)).appendTo(div);
    //topDiv.appendTo(div);
    //bottomDiv.appendTo(div);
    if (!(opt.group in option_groups)) {
        option_groups[opt.group] = {
            button: $('<a href="#" class="toolbutton disabled when_started"><span class="group_name">' + opt.group + '</span><span class="ui-icon ui-icon-triangle-1-s"></span></a>'),
            dropdown: $('<div class="option_group ui-widget">' + (opt.group === "Search" ? "" : '<span class="group_header">' + opt.group + ' options</span>') + '</div>'),
        };
    }
    div.appendTo(option_groups[opt.group].dropdown);
}

function check_hover (button, dropdown, which) {
    return function (event, ui) {
        if (which) {
            $(this).addClass("hover");
        } else {
            $(this).removeClass("hover");
        }

        /* This has to be in a timeout handler to allow for transitioning between the
           button and dropdown without hiding it; plus it feels more natural */
        window.setTimeout(function () {
            if (button.filter(".hover:not(:disabled):not(.ui-state-disabled)").length > 0
                || dropdown.filter(".hover:not(:disabled):not(.ui-state-disabled)").length > 0
                /* for debugging purposes */
                || button.filter(".force_hover").length > 0
                || dropdown.filter(".force_hover").length > 0) {

                /* don't re-show dropdown if visible to avoid accidentally moving it */
                if (!dropdown.is(":visible")) {
                    dropdown.show();
                    dropdown.position({
                        my: "left top",
                        at: "left bottom-2",
                        of: button,
                        collision: "flip none"
                    });
                    button.addClass("active");
                }
            } else {
                dropdown.hide();
                button.removeClass("active");
            }
        }, 5);
    }
}

for (var group_name in option_groups) {
    var group = option_groups[group_name];
    var enter = check_hover(group.button, group.dropdown, true);
    var leave = check_hover(group.button, group.dropdown, false);
    group.button
        .mouseenter(enter).mouseleave(leave);
    group.dropdown
        .mouseenter(enter).mouseleave(leave);
    /* special case search options to go in the search command dropdown */
    if (group_name === "Search") {
        group.dropdown.children().prependTo("#search_dropdown");
        ko.applyBindings(STATE, $("#search_dropdown")[0]);
    } else {
        group.button.appendTo($("#top_bar_options"));
        group.dropdown.appendTo($("body"));
        ko.applyBindings(STATE, group.dropdown[0]);
    }
}

[
    {
        button: $("#search_button").eq(0),
        dropdown: $("#search_dropdown").eq(0)
    },
    {
        button: $("#link_button").eq(0),
        dropdown: $("#link_dropdown").eq(0)
    }

].forEach(function (group) {
    var enter = check_hover(group.button, group.dropdown, true);
    var leave = check_hover(group.button, group.dropdown, false);
    group.button
        .mouseenter(enter).mouseleave(leave);
    group.dropdown
        .mouseenter(enter).mouseleave(leave);
});

STATE.isa_name.subscribe(function (value) {
    if ($("#options").dialog("instance")) {
        $("#options").dialog("option", "title", "Model options (" + value + ")");
    }
});
