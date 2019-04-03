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
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

function do_command (cmd, log) {
    if (log) {
      print("<p class=rmem>" + STATE.prompt() + "&nbsp;<b class=rmem_cmd>" + escape_html(cmd) + "</b></p>");
    }
    STATE.prompt("(...)");
    $(".interact_loading").show();
    graph_outdated();
    interact_lib.input_str(cmd);
}

function create_ui (root) {
    var el = $(root);
    el.find(".tabs").tabs();
    el.find(".controlgroup, .toolbar").controlgroup();
    $(".dot").panzoom();
}

$(document).ready(function () {
    $(document).on("click", ".cmdbutton:not(.disabled)", function (e) {
        do_command($(this).attr("cmd"), true);
    });

    $(document).on("click", "#restart", function (e) {
        restart();
    });

    $(document).on("click", ".help_button", function (e) {
        var extant_helps = $(".help_loader");
        if (extant_helps.length > 0) {
            extant_helps.effect("highlight");
        } else {
            switch_panes($(".pane").eq(0), "help");
            ko.tasks.schedule(function() {
                $(".help_loader").effect("highlight");
            });
        }
    });

    $(document).on("click", "a.toolbutton", function (e) {
        if (!$(this).is(".follow")) {
            e.preventDefault();
            return false;
        }
    });

    $(document).on("click", ".help_loader a", function (e) {
        var el = $(this);
        if (el.attr("href").startsWith("#")) {
            el.closest(".help_loader").find(el.attr("href"))[0].scrollIntoView(true);
            e.preventDefault();
            return false;
        }
    });

    $(document).on("click", ".option .top", function (e) {
        var that = $(this);
        if (that.find("input[type=checkbox]").length > 0) {
            var target = $(e.target);
            var parents = target.parents();
            if (!that.hasClass("disabled")
                && !target.is("input") && !parents.is("input") && !target.is("button") && !parents.is("button")) {
                var checkbox = that.find("input[type=checkbox]");
                checkbox.prop("checked", !checkbox.prop("checked")).trigger("change");
                return false;
            }
        }
    });

    $(document).on("click", "span.follow_list", function (e) {
        do_command("set follow_list " + $(this).text(), true);
        e.preventDefault();
        return false;
    });

    $(document).on("click", ".rmem_cmd", function (e) {
        do_command($(this).text(), true);
        e.preventDefault();
        return false;
    });

    $("#link_copy_button").click(function () {
        if (STATE.test_type() === "litmus") {
            var total_history = interact_lib.get_history();
            var i;
            for (i = total_history.length; i > 0; i--) {
                // skip backwards until the first non-number -> non-transition
                if (!total_history[i - 1].match(/^\d+$/)) {
                    break;
                }
            }

            var command_history = total_history.slice(0, i);
            var follow_history = total_history.slice(i);


            var obj = {
                test: load_litmus_editor.getValue().trim()
            };

            if ($("#link_follow_list").prop("checked")) {
                obj.follow = follow_history.join(",");
                if ($("#link_include_commands").prop("checked")) {
                    obj.history = command_history.join(";");
                }
            } else {
                if ($("#link_include_commands").prop("checked")) {
                    obj.history = total_history.join(";");
                }
            }

            if ($("#link_model_options").prop("checked")) {
                obj.model_options = {
                    model: $("input[type='radio'][name='model']:checked").val(),
                    embedding: $("input[type='radio'][name='semantics']:checked").val(),
                    force_sc: $("input[type='radio'][name='force_sc']:checked").val(),
                    tree_speculation: $("input[type='radio'][name='tree_speculation']:checked").val(),
                    promise_first: $("input[type='radio'][name='promise_first']:checked").val(),
                    flowing_topology_2: $("input[type='radio'][name='topology_2']:checked").val(),
                    flowing_topology_3: $("input[type='radio'][name='topology_3']:checked").val(),
                    flowing_topology_4: $("input[type='radio'][name='topology_4']:checked").val()
                };
            }

            if ($("#link_all_options").prop("checked")) {
                obj.options = last_options;
            }

            if ($("#link_window_layout").prop("checked")) {
                obj.panes = serialize_split();
            }

            var path = window.location.toString().split("#")[0];

            // TODO FIXME TEMPORARY
            if ($("#link_use_redirect").prop("checked")) {
                path = path.replace("/~jf451/", "/~pes20/");
            }

            var link = path + "#" + encodeURIComponent(JSON.stringify(obj));

            var fallback = function (url) {
                return function (err) {
                    error_dialog("Could not copy to clipboard. Error:\n\n'" + err.message
                                 + "'\n\nHere is the link, please copy it manually.\n\n"
                                 + '<a href="' + url + '" target="_blank">' + url + '</a>\n\n');
                }
            };

            if ($("#link_shorten_url").prop("checked")) {
                confirm_dialog("Creating a short URL leaks potentially-confidential\n"
                               + "information, e.g. your litmus test, to the operators\n"
                               + 'of the URL shortening service (in our case, <a href="http://is.gd/" target="_blank">is.gd</a>).\n\n'
                               + "Are you sure you want to do this?",
                               function () {
                                   var close_loading_dialog = processing_dialog("Requesting short URL...");
                                   var show_error = function (msg) {
                                       error_dialog("Error requesting short URL: '" + msg + "'");
                                   };
                                   $.ajax({
                                       url: "https://is.gd/create.php?format=json&url=" + encodeURIComponent(link),
                                       method: "GET",
                                       crossDomain: true,
                                       dataType: "jsonp",
                                       cache: false,
                                       timeout: 10000,
                                       success: function (data, text_status, jqXHR) {
                                           if (data.shorturl) {
                                               message_dialog("Short URL created:\n\n"
                                                              + '<a href="' + data.shorturl + '" target="_blank">' + data.shorturl + '</a>\n\n'
                                                              + "Click OK to copy to clipboard.",
                                                              function () {
                                                                  clipboard.copy(data.shorturl).catch(fallback(data.shorturl));
                                                              });
                                           } else {
                                               show_error(data.errormessage);
                                           }
                                       },
                                       error: function (jqXHR, text_status, error_thrown) {
                                           show_error(text_status + (error_thrown ? " - " + error_thrown : ""));
                                       },
                                       complete: close_loading_dialog,
                                   });
                               });
            } else {
                clipboard.copy(link).catch(fallback(link));

                var notif = $("#link_dropdown .notification");
                notif.css("visibility", "visible").css("opacity", 1);
                notif.effect("highlight", { color: "gold" });
                notif.animate({ opacity: 0 }, 500, "swing", function () {
                    notif.css("visibility", "hidden");
                });
            }
        } else {
            window.alert("Sorry, currently only litmus tests can be linked to");
        }
    });

    $(document).on("click", ".trans", function () {
        do_command($(this).attr("id"), true);
    });


    $(document).on("keypress", ".input_cmd", function (e) {
        if (e.keyCode === 13) { // enter/newline
            do_command($(this).val(), true);
            $(this).val("");
            e.preventDefault();
            return false;
        } else {
            e.stopPropagation();
            return true;
        }
    });

    $("body").keypress(function (e) {
        // Don't redirect keypresses if we're already in a prompt...
        if ($(document.activeElement).hasClass("input_cmd")
            // ...or there is some other focused element which is an
            // <input type="text"> or <textarea>
            || (document.activeElement !== null && document.activeElement !== undefined &&
                ((document.activeElement.tagName.toLowerCase() === "input" &&
                  document.activeElement.hasAttribute("type") &&
                  (document.activeElement.attributes["type"].value.toLowerCase() === "text" ||
                   document.activeElement.attributes["type"].value.toLowerCase() === "number")) ||
                 document.activeElement.tagName.toLowerCase() === "textarea"))
           ) {
            return true;
        } else {
            var textarea = $(".input_cmd").eq(0);
            textarea.val(textarea.val() + String.fromCharCode(e.which));
            textarea.focus();
            return false;
        }
    });

    $(document).on("click", ".font_size_reset", function () {
        $(this).closest(".mid_bar").parent().find(".adjust_font_size, .CodeMirror").css("font-size", "0.9em");
        update_editors($(this).closest(".pane"));
    });

    $(document).on("click", ".font_size_plus", function () {
        var el = $(this).closest(".mid_bar").parent().find(".adjust_font_size, .CodeMirror");
        el.css("font-size", parseFloat(el.eq(0).css("font-size").slice(0, -2)) * 1.1);
        update_editors($(this).closest(".pane"));
    });

    $(document).on("click", ".font_size_minus", function () {
        var el = $(this).closest(".mid_bar").parent().find(".adjust_font_size, .CodeMirror");
        el.css("font-size", parseFloat(el.eq(0).css("font-size").slice(0, -2)) * 0.9);
        update_editors($(this).closest(".pane"));
    });

    $(document).on("click", ".litmus_download_file", function () {
        var test = STATE.sources()[0].content.trim();
        if (test.length > 0) {
            var blob = new Blob([test + "\n"], {type:'text/plain'});

            var downloadLink = document.createElement("a");
            var name = STATE.sources()[0].name.split(/\s+/)[1];
            downloadLink.download = name + ".litmus";
            var url = window.URL.createObjectURL(blob);
            downloadLink.href = url;
            document.body.appendChild(downloadLink);
            downloadLink.click();
            setTimeout(function() {
              document.body.removeChild(downloadLink);
              window.URL.revokeObjectURL(url);
            }, 0);
        } else {
            error_dialog("Please enter a litmus test.");
        }
    });
});

create_ui($(document));
