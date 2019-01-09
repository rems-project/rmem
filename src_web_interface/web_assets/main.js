/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Jon French, University of Cambridge  2017                          */
/*  Copyright Shaked Flur, University of Cambridge 2017                          */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  */
/*  For author information see README.md.                                        */
/*                                                                               */
/*===============================================================================*/

var started = false;
var scroll_anyway = false;

function show_prompt (str) {
    $(".interact_prompt").css("display", "inline-block");
    STATE.prompt(str + ":");
    $(".interact_loading").hide();
    $(".input_cmd").eq(0).focus();
    $(".input_cmd").each(function () {
        this.scrollIntoView(false);
    });
}

function println (s) {
    STATE.console_lines.push(s);
    $(document).ready(function() {
        if (scroll_anyway || $("#scroll_on_output-toggle").prop("checked")) {
            scroll_anyway = false;
            ko.tasks.schedule(function () {
                $(".system_state_lines > :last-child").each(function () {
                    this.scrollIntoView(false);
                });
            });
        }
    });
}

function update_transition_history (history, available) {
    STATE.trace_lines.removeAll();
    history.split("\n").forEach(function(x) {
        STATE.trace_lines.push(x);
    });
    STATE.trace_available_transitions(available);
}

function clear_screen () {
    STATE.console_lines.removeAll();
}

function quit () {
    // what to do here?
}

function restart () {
    var options = last_options;
    if (STATE.test_type() === "litmus") {
        load_litmus();
        set_all_options(options);
    } else if (STATE.test_type() === "elf") {
        load_elf();
        set_all_options(options);
    } else {
        error_dialog("Unknown test_type, cannot restart");
    }
}

function set_isa (isa_name) {
    STATE.isa_name(isa_name);
}

function set_model (model_name) {
    STATE.model_name(model_name);
}

function default_split () {
    restore_split({
        horizontal: {
            sizes: [50, 50],
            contents: [
                {
                    vertical: {
                        sizes: [50, 50],
                        contents: [
                            {
                                pane: "console"
                            },
                            {
                                pane: "help"
                            }
                        ]
                    }
                },
                {
                    vertical: {
                        sizes: [50, 50],
                        contents: [
                            {
                                pane: "graph"
                            },
                            {
                                pane: "sources"
                            }
                        ]
                    }
                }
            ]
        }
    });
}

function hash_changed () {
    var obj = JSON.parse(decodeURIComponent(window.location.hash.substr(1)));
    if (obj.model_options) {
        $("input[type='radio'][name='model'][value='" + obj.model_options.model + "']").prop("checked", true);
        $("input[type='radio'][name='embedding'][value='" + obj.model_options.embedding + "']").prop("checked", true);
        $("input[type='radio'][name='force_sc'][value='" + obj.model_options.force_sc + "']").prop("checked", true);
        $("input[type='radio'][name='tree_speculation'][value='" + obj.model_options.tree_speculation + "']").prop("checked", true);
        $("input[type='radio'][name='flowing_topology_2'][value='" + obj.model_options.flowing_topology_2 + "']").prop("checked", true);
        $("input[type='radio'][name='flowing_topology_3'][value='" + obj.model_options.flowing_topology_3 + "']").prop("checked", true);
        $("input[type='radio'][name='flowing_topology_4'][value='" + obj.model_options.flowing_topology_4 + "']").prop("checked", true);
    }
    load_litmus_editor.setValue(obj.test);
    load_litmus();
    if (obj.options) {
        set_all_options(obj.options);
    }
    if (obj.history && obj.history.length > 0) {
        interact_lib.input_str(obj.history);
    }
    if (obj.follow && obj.follow.length > 0) {
        interact_lib.input_str("set follow_list " + obj.follow);
    }
    if (obj.panes) {
        restore_split(obj.panes);
    } else {
        default_split();
    }
}

$(document).ready(function () {
    $(window).on("hashchange", hash_changed);

    window.onerror = function (e) {
        var error_str = '<span class="error">Fatal error: Caught exception ' + e.toString() + "</span>";
        // scroll into view even if scroll-on-output is off
        scroll_anyway = true;
        println(error_str);
    }
});

webppc_lib.load();
STATE.version(("r" + webppc_lib.get_version()).replace(" (clean)", ""));
STATE.build_date("Last changed: " + webppc_lib.get_last_changed());

if (window.location.hash) {
    hash_changed();
} else {
    default_split();
}
