/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Jon French, University of Cambridge       2017                     */
/*  Copyright Shaked Flur, University of Cambridge 2017-2018                     */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

var load_litmus_dialog = $("#load_litmus_dialog").dialog({
    autoOpen: false,
    modal: true,
    width: 450,
    height: Math.min(700, window.innerHeight - 40),
    buttons: {
        "Cancel": function () {
            $(this).dialog("close");
        },
        "OK": load_litmus
    },
    open: function () {
        load_litmus_editor.refresh();
    },
    close: clear_this_errors
});

var litmus_library_dialog = $("#litmus_browser").dialog({
    autoOpen: false,
    modal: true,
    width: 1000,
    height: Math.min(750, window.innerHeight - 40)
});

var elf_library_dialog = $("#elf_browser").dialog({
    autoOpen: false,
    modal: true,
    width: 1000,
    height: Math.min(750, window.innerHeight - 40)
});

var load_elf_dialog = $("#load_elf_dialog").dialog({
    autoOpen: false,
    modal: true,
    width: 550,
    buttons: {
        "Cancel": function () {
            $(this).dialog("close");
        },
        "OK": load_elf
    },
    close: clear_this_errors
});

var load_litmus_editor = CodeMirror.fromTextArea(load_litmus_dialog.find("textarea")[0], {
    lineNumbers: true,
    electricChars: false,
    mode: { name: "litmus" }
});

var litmus = get_cookie("litmus");
if (litmus) {
    load_litmus_editor.setValue(litmus);
}

function fetch_litmus_test (litmus) {
    var close_loading_dialog = processing_dialog("Loading test...");
    return jqxhr_to_promise($.ajax({
        url: litmus,
        type: "GET",
        dataType: "text",
        cache: false
    })).then(function (test) {
        load_litmus_editor.setValue(test);
        close_loading_dialog();
        litmus_library_dialog.dialog("close");
        load_litmus();
    }).catch(jqxhr_to_exc(function (err) {
        close_loading_dialog();
        error_dialog("Error loading litmus test: " + err.message);
    }));
}

function fetch_elf_test (name, binary_url, source_urls, threads) {
    var close_loading_dialog = processing_dialog("Loading test...");
    return jqxhr_to_promise($.ajax({
        url: binary_url,
        type: "GET",
        dataType: "native",
        xhrFields: {
            responseType: "arraybuffer"
        },
        cache: false
    })).then(function (buf) {
        var sources = [];
        return Promise.all(source_urls.map(function (url) {
            var parts = url.split("/");
            return jqxhr_to_promise($.ajax({
                url: url,
                type: "GET",
                dataType: "text",
                cache: false
            })).then(function (data) {
                sources.push({ name: parts[parts.length - 1], content: data });
            });
        })).then(function () {
            $("#use_dwarf").prop("checked", true);
            $("#elf_threads").val(threads);
            webppc_lib.set_elf_data(name, new Uint8Array(buf), buf.byteLength);
            STATE.sources(sources);
            close_loading_dialog();
            elf_library_dialog.dialog("close");
            load_elf_dialog.dialog("close");
            load_elf();
        });
    }).catch(jqxhr_to_exc(function (err) {
        error_dialog("Error loading ELF test: " + err.message);
        close_loading_dialog();
    }));
}

function load_test_library(dialog_root, library_name, friendly_name, accessor) {
    var close_loading_dialog = processing_dialog("Loading " + friendly_name + " library...");
    return jqxhr_to_promise($.ajax({
        url: library_name + "_library.json",
        type: "GET",
        dataType: "json",
        cache: false
    })).then(function (data) {
        accessor(data);
        ko.applyBindings(STATE, dialog_root[0]);
        return ko.tasks.schedule_promise().then(function () {
            return Promise.all(data.map(function (item) {
                return Promise.all(item.categories.map(function (category) {
                    if (category.tests) {
                        var node = document.importNode(dialog_root.find(".contents_template")[0].content, true).firstElementChild;
                        ko.applyBindings({
                            category: category,
                            tests: category.tests
                        }, node);
                        return node;
                    } else {
                        return jqxhr_to_promise($.ajax({
                            url: "tests/" + item.arch + "/" + category.folder,
                            type: "GET",
                            dataType: "text",
                            cache: false
                        })).then(function (data) {
                            var tests = data.split("\n").filter(function (line) {return line.trim().charAt(0) !== '#';});
                            var node = document.importNode(dialog_root.find(".contents_template")[0].content, true).firstElementChild;
                            ko.applyBindings({
                                tests: tests.map(function (test) {
                                    return {
                                        // remove path and suffix (.litmus) from the file
                                        name: test.split("/").pop().slice(0,-7),
                                        url: "tests/" + item.arch + "/" + test
                                    };
                                }),
                                category: category
                            }, node);
                            return node;
                        }).catch(jqxhr_to_exc(function (err) {
                            var node = document.importNode(dialog_root.find(".error_template")[0].content, true).firstElementChild;
                            ko.applyBindings({
                                category: category,
                                error: "Error loading tests: " + err.message
                            }, node);
                            return node;
                        }));
                    }
                })).then(function (nodes) {
                    var after = $("#" + item.arch + "_" + library_name + "_browser > .clear");
                    nodes.forEach(function (node) {
                        after.before(node);
                    });
                });
            }));
        });
    }).catch(jqxhr_to_exc(function (err) {
        error_dialog("Error loading litmus library: " + err.message);
    })).then(function () {
        var show_first = function (panel) {
            ko.tasks.schedule_promise().then(function () {
                panel.find(".category_wrapper > .category > ul > li").eq(0).mouseover();
            });
        };
        dialog_root.find(".litmus_tabs").tabs({
            beforeActivate: function (event, ui) { show_first(ui.newPanel); },
            create: function (event, ui) { show_first(ui.panel); }
        });
        close_loading_dialog();
    });
}


function load_common(typ, name) {
    $("#interact_loading").show();
    if (name) {
        STATE.test_name(name);
    } else {
        STATE.test_name("(no test loaded)");
    }
    $(document).ready(function() {
        $(".when_started").show();
        $(".when_started").removeClass("disabled");
        $("button.when_started, input[type=button].when_started").prop("disabled", false);
    });
    started = true;
    STATE.test_type(typ);
}

function unset_elf () {
    webppc_lib.unset_elf_data();
    $("#elf_dialog_current_file").text("(none)");
}

function load_litmus () {
    var test = load_litmus_editor.getValue().trim();
    if (test.length > 0) {
        set_cookie("litmus", test);
        var name = test.split("\n")[0];
        STATE.sources([{ name: name, content: test }]);
        load_common("litmus", name);
        unset_elf();
        load_litmus_dialog.dialog("close");
        webppc_lib.start_interactive_litmus(name, test);
    } else {
        error_dialog("Please enter a litmus test.");
    }
}

function load_elf () {
    var name = webppc_lib.elf_file_name();
    if (name != null) {
        load_common("elf", name);
        load_elf_dialog.dialog("close");
        webppc_lib.start_interactive_elf();
    } else {
        $("#file_select").addClass("ui-state-error");
        error_dialog("Please select an ELF file.");
    }
}

var filterTimer = null;

$(document).ready(function () {
    $(document).on("input", ".litmus_filter", function() {
        var self = this;
        clearTimeout(filterTimer);
        filterTimer = setTimeout(function() {
          var filter = $(self).val();
          if (filter === "") {
            $(self).closest(".litmus_browser").find(".litmus_name").each(function() {
              $(this).removeClass("filter_miss");
              $(this).removeClass("filter_match");
            });

            $(self).closest(".litmus_browser").find(".category_wrapper > .category > ul > li").each(function() {
                $(this).removeClass("filter_match");
                $(this).removeClass("filter_miss");
            });
          } else {
            $(self).closest(".litmus_browser").find(".litmus_name").each(function() {
              if ($(this).text().indexOf( filter ) === -1) {
                  $(this).removeClass("filter_match");
                  $(this).addClass("filter_miss");
              } else {
                  $(this).removeClass("filter_miss");
                  $(this).addClass("filter_match");
              }
            });

            $(self).closest(".litmus_browser").find(".category_wrapper > .category > ul > li").each(function() {
              if ($(this).closest(".litmus_browser").find(".contents[folder=\"" + $(this).attr("folder") + "\"] .filter_match").length > 0) {
                $(this).removeClass("filter_miss");
                $(this).addClass("filter_match");
              } else {
                $(this).removeClass("filter_match");
                $(this).addClass("filter_miss");
              }
            });
          }
        }, 750);
    });

    $(document).on("mouseenter", ".litmus_browser > .category_wrapper > .category > ul > li", function() {
        $(this).parent().children("li").removeClass("selected");
        $(this).addClass("selected");
        $(this).closest(".litmus_browser").find(".contents").hide();
        $(this).closest(".litmus_browser").find('.contents[folder="' + $(this).attr("folder") + '"]').show();
    });

    // make scrollable
    $(document).on("mousemove", ".litmus_browser > .category_wrapper", function(e) {
        var inactive_margin = 50;
        var wrapper_height = $(this).height();
        var scrollable_height = $(this).children(".category").outerHeight() + 2 * inactive_margin;
        var wrapper_offset = $(this).offset();
        var top = (e.pageY -  wrapper_offset.top) * (scrollable_height - wrapper_height) / wrapper_height  - inactive_margin;

        if (top < 0)
            top = 0;

        $(this).scrollTop(top);
    });

    $("#elf_file_select").on("change", function(e) {
        var file = e.target.files[0];
        if (!file) {
            return;
        }

        var reader = new FileReader();

        reader.onloadend = function(e) {
            var arrayBuffer = e.target.result;
            webppc_lib.set_elf_data(file.name, new Uint8Array(arrayBuffer), arrayBuffer.byteLength);
            $("#elf_dialog_current_file").text(file.name);
        };
        reader.readAsArrayBuffer(file);

        $(this).removeClass("ui-state-error");
    });

    $("#elf_source_select").on("change", function(e) {
        var deferreds = [];
        for (var i = 0; i < e.target.files.length; i++) {
            // I hate Javascript and its lack of proper scope.
            // So much. This took so long to completely unnecessarily debug.
            (function(){
                var file = e.target.files[i];
                var reader = new FileReader();
                var deferred = jQuery.Deferred();
                reader.onloadend = function(e) {
                    deferred.resolve([file.name, e.target.result]);
                };
                reader.readAsText(file);
                deferreds.push(deferred);
            })();
        }
        jQuery.when.apply(this, deferreds).done(function() {
            if (arguments.length > 0 && arguments[0] !== undefined) {
                for (var i = 0; i < arguments.length; i++) {
                    STATE.sources.push({ name: arguments[i][0], content: arguments[i][1] });
                }
            }
        });
    });

    $("#litmus_file_select").on("change", function(e) {
        var file = e.target.files[0];
        if (!file) {
            return;
        }

        var reader = new FileReader();
        reader.onloadend = function(e) {
            load_litmus_editor.setValue(e.target.result);
        };
        reader.readAsText(file);
    });


    $("#load_litmus_button").click(function(e) {
        load_litmus_dialog.dialog("open");
    });

    $("#load_test_button").click(function(e) {
        load_elf_dialog.dialog("open");
    });

    $(document).on("click", "#litmus_browser .litmus_name", function () {
        fetch_litmus_test($(this).attr("test_url"));
    });

    $(document).on("click", "#elf_browser .litmus_name", function () {
        var that = $(this);
        fetch_elf_test(that.attr("test_name"), that.attr("binary_url"), JSON.parse(that.attr("source_urls")), that.attr("n_threads"));
    });

    $("#litmus_from_library").click(function () {
        if (STATE.litmus_library().length < 1) {
            load_test_library($("#litmus_browser"), "litmus", "litmus", STATE.litmus_library).then(function () {
                litmus_library_dialog.dialog("open");
            });
        } else {
            litmus_library_dialog.dialog("open");
        }
    });

    $("#elf_from_library").click(function () {
        if (STATE.elf_library().length < 1) {
            load_test_library($("#elf_browser"), "elf", "ELF", STATE.elf_library).then(function () {
                elf_library_dialog.dialog("open");
            });
        } else {
            elf_library_dialog.dialog("open");
        }
    });

    $("#litmus_from_file").click(function () {
        $("#litmus_file_select").trigger("click");
    });

    $("#elf_from_file").click(function () {
        $("#elf_file_select").trigger("click");
    });

    $("#elf_source_add").click(function () {
        $("#elf_source_select").val("");
        $("#elf_source_select").trigger("click");
    });

    $("#elf_source_remove").click(function () {
        var to_remove = $("#elf_source_list").val();
        STATE.sources(STATE.sources().filter(function (source) {
            return (to_remove.indexOf(source.name) === -1);
        }));
    });
});
