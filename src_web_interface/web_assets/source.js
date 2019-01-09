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

var codemirrors = [];

function create_code_element (name, code, place) {
    var mode = null;
    var lower_name = name.toLowerCase();
    if (STATE.test_type() === "litmus") {
        mode = "litmus";
    } else {
        if (lower_name.endsWith(".s") || lower_name.endsWith(".asm")) {
            mode = { name: "gas", architecture: "arm" };
        } else if (lower_name.endsWith(".c") || lower_name.endsWith(".h")) {
            mode = "text/x-c";
        }
    }
    return CodeMirror(place, {
        lineNumbers: true,
        electricChars: false,
        readOnly: "nocursor",
        value: code,
        mode: mode
    });
}

STATE.sources.subscribe(function (files) {
    var tab_container = $(".source_tabs");
    var tab_headers = $(".source_tabs > ul");
    var tab_num = 0;

    $(".source_tabs").tabs("refresh");

    interact_lib.update_sources(files);
});

function update_editors (pane) {
    $(pane).find(".source_tab").each(function () {
        var codemirror = $(this).data("codemirror");
        if (codemirror) {
            codemirror.refresh();
        }
    });
}

var tab_id = 0;
ko.bindingHandlers.source_tab = {
    init: function (element, value_accessor, all_bindings) {
        var el = $(element);
        var id = el.attr("id");
        if (id === undefined) {
            id = "source_tab_" + (tab_id++).toString();
            el.attr("id", id);
        }
        var value = ko.unwrap(value_accessor());
        var li = $('<li><a href="#' + id + '"></a></li>');
        el.data("tab_li", li);
        el.siblings("ul").append(li);
        var tab_container = el.closest(".source_tabs");
        if (!tab_container.tabs("instance")) {
            tab_container.tabs();
        }
        tab_container.tabs("refresh").tabs("option", "active", 0);

        ko.utils.domNodeDisposal.addDisposeCallback(element, function () {
            li.remove();
            tab_container.tabs("refresh");
        });
    },
    update: function (element, value_accessor, all_bindings) {
        var value = ko.unwrap(value_accessor());
        var el = $(element);
        el.empty();
        el.data("tab_li").find("a").text(value.name);
        el.data("codemirror", create_code_element(value.name, value.content, el[0]));
        el.closest(".source_tabs").tabs("refresh").tabs("option", "active", 0);
    }
};

$(document).on("tabsactivate", ".source_tabs", function (e, ui) {
    var codemirror = ui.newPanel.data("codemirror");
    if (codemirror) {
        codemirror.refresh();
    }
});
