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

var splitter = null;
var right_splitter = null;

function element_style_func (dimension, size, gutter_size) {
    return {
        "flex-basis": "calc(" + size + "% - " + gutter_size + "px)",
        "flex-shrink": 1,
        "flex-grow": 1
    };
}

function gutter_style_func (dimension, gutter_size) {
    return {
        "flex-basis":  gutter_size + "px",
        "flex-shrink": 0,
        "flex-grow": 0
    };
}

function fix_splitter_sizes(splitter) {
    var sizes = splitter.getSizes();
    var sum = sizes.reduce(function(a, b) { return a + b }, 0);
    if (sum > 100.0) {
        var diff = sum - 100.0;
        for (var i = 0; i < sizes.length; i++) {
            sizes[i] -= diff;
        }
        splitter.setSizes(sizes);
    }
}

function create_splitter (element, sizes) {
    var el = $(element);
    el.addClass("split");
    if (!(el.hasClass("horizontal") || el.hasClass("vertical"))) {
        el.addClass("horizontal");
    }
    if (sizes === undefined) {
        sizes = [50, 50];
    }
    var direction = (el.hasClass("vertical") ? "vertical" : "horizontal");
    var children = el.children();
    var one = children[0];
    var two = children[1];
    var split = Split([one, two], {
        elementStyle: element_style_func,
        gutterStyle: gutter_style_func,
        sizes: sizes,
        direction: direction,
        gutterSize: 5,
        onDrag: function () {
            update_editors(one);
            update_editors(two);
        }
    });
    el.data("splitter", split);
    return split;
}

function split (element, direction) {
    var el = $(element);
    if (direction != "vertical") {
        direction = "horizontal";
    }
    var parent = el.parent();
    var parent_splitter = null;
    var previous_sizes = null;
    if (parent.hasClass("split")) {
        var parent_splitter = parent.data("splitter");
        var previous_sizes = parent_splitter.getSizes();
        parent_splitter.destroy();
    }
    el.wrap('<div class="split ' + direction + '"></div>');
    var new_splitter = el.parent();
    var new_sibling = $('<div data-bind="' + el.attr("data-bind") + '" class="' + el.attr("class") + '"></div>');
    new_splitter.append(new_sibling);
    ko.applyBindings(STATE, new_sibling[0]);
    ko.tasks.schedule(function() {
        create_ui(new_sibling);
    });
    if (parent.hasClass("split")) {
        fix_splitter_sizes(create_splitter(parent, previous_sizes));
    }
    create_splitter(new_splitter);
    el.find(".split_close_button").removeClass("disabled");
}

function close_pane (element) {
    var el = $(element);
    var parent = el.parent();
    var upper = parent.parent();
    var upper_splitter = null;
    var previous_sizes = null;
    if (upper.hasClass("split")) {
        upper_splitter = upper.data("splitter");
        previous_sizes = upper_splitter.getSizes();
    }
    parent.data("splitter").destroy();
    var sibling = $(el.siblings()[0]);
    el.remove();
    parent.replaceWith(sibling);
    if (upper.hasClass("split") || sibling.hasClass("split")) {
        upper_splitter.destroy();
        fix_splitter_sizes(create_splitter(upper, previous_sizes));
    } else {
        $(sibling).find(".split_close_button").addClass("disabled");
    }
}

function switch_panes (previous_pane, which) {
    var new_pane = $('<div data-bind="component: \'' + which + '\'" class="' + which + ' pane"></div>');
    var parent = previous_pane.parent();
    var previous_splitter = null;
    var previous_sizes = null;
    var new_splitter = null;
    if (parent.hasClass("split")) {
        previous_splitter = parent.data("splitter");
        previous_sizes = previous_splitter.getSizes();
        previous_splitter.destroy();
    }
    previous_pane.replaceWith(new_pane);
    if (parent.hasClass("split")) {
        new_splitter = create_splitter(parent, previous_sizes);
    }
    ko.applyBindings(STATE, new_pane[0]);
    ko.tasks.schedule(function() {
        create_ui(new_pane);
        if (new_splitter) {
            fix_splitter_sizes(new_splitter);
        }
    });
}

function restore_split (data) {
    function impl (data) {
        $(".split_loading").remove();
        if (data["horizontal"] || data["vertical"]) {
            var direction = (data["horizontal"] ? "horizontal" : "vertical");
            var split = data[direction];
            var div = $('<div class="split ' + direction + '"></div>');
            split.contents.forEach(function (child) {
                impl(child).appendTo(div);
            });
            create_splitter(div, split.sizes);
            return div;
        } else if (data["pane"]) {
            var pane = data["pane"];
            var div = $('<div class="' + pane + ' pane" data-bind="component: \'' + pane + '\'"></div>');
            div.data("pane_options", pane.options);
            return div;
        } else {
            // unknown
        }
    }

    var root = $("#split_root");
    root.empty();
    impl(data).appendTo(root);
    ko.applyBindingsToDescendants(STATE, root[0]);
    ko.tasks.schedule(function () {
        create_ui(root);
    });
}

function serialize_split () {
    function impl (index, _root) {
        var root = $(_root);
        if (root.hasClass("horizontal")) {
            return {
                horizontal: {
                    sizes: root.data("splitter").getSizes(),
                    contents: root.children().filter(":not(.gutter)").map(impl).get()
                }
            }
        } else if (root.hasClass("vertical")) {
            return {
                vertical: {
                    sizes: root.data("splitter").getSizes(),
                    contents: root.children().filter(":not(.gutter)").map(impl).get()
                }
            }
        } else if (root.hasClass("pane")) {
            return {
                /* whether or not this is an 'internal undocumented' Knockout API is unclear */
                pane: ko.bindingProvider["instance"].getBindings(root[0], new ko.bindingContext(STATE))["component"]
            }
        } else {
            // unknown
        }
    }

    return impl(0, $("#split_root").children().eq(0));
}

$(document).ready(function () {
    $(document).on("click", ".split_horizontal_button", function () {
        split($(this).closest(".pane"), "horizontal");
    });

    $(document).on("click", ".split_vertical_button", function () {
        split($(this).closest(".pane"), "vertical");
    });

    $(document).on("click", ".split_close_button", function () {
        close_pane($(this).closest(".pane"));
    });
});

$(".split").each(function () {
    create_splitter(this);
});
