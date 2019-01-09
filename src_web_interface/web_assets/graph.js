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

var worker = null;
var dot_callbacks = {};
var next_callback_id = 0;

worker = new Worker("web_assets/worker.js");
worker.onmessage = function (e) {
    call_dot_callback(e.data.callback_id, e.data.output);
};
worker.onerror = function (e) {
    console.log(e);
    throw e;
};

function graph_outdated() {
    $(".dot_outdated").show();
    $(".graph g.node text")
        .css("text-decoration", "none")
        .css("cursor", "inherit")
        .off("click");
}

function do_display_dot(html) {
    STATE.graph_html(html);
    ko.tasks.schedule(function () {
        $(".graph g.node text[fill='#0000ff'], .graph g.node text[fill='#006400']").each(function (i) {
            var s = $(this).text();
            if (s.length > 0 && /^\d+:/.test(s) && !/^\d+:\d+ /.test(s)) {
                $(this).css("text-decoration", "underline")
                    .css("cursor", "pointer")
                    .click(function(event) {
                        do_command($(this).text().split(":")[0]);
                        event.stopPropagation();
                        return false;
                    });
            }
        });
        $(".graph g.node text").each(function (i) {
            /*
              HACK: Ensure the text fits the tables...

              This is needed because Graphviz has no way of knowing
              precisely what font the final SVG representation of
              the graph will be rendered against, despite our
              best efforts to ensure consistency.

              The hack is: if a text element inside a graph node is
              preceded by a polygon, assume that's its containing cell
              and check it fits within, scaling it if necessary using
              SVG text stretch.

              The problem mainly seems to affect bold text.
            */
            var that = $(this);
            var text = that[0];
            var poly = $(this).prev();
            if (poly.length == 1) {
                if (poly[0].tagName === "polygon") {
                    var poly_bbox = poly[0].getBBox();
                    var text_bbox = text.getBBox();
                    if (text_bbox.width > poly_bbox.width) {
                        var new_length = poly_bbox.width - 2 * (text_bbox.x - poly_bbox.x);
                        text.setAttribute("textLength", new_length);
                    }
                }
            }
        });
    });
    $(".dot_loading").hide();
}

function add_dot_callback(callback) {
    var callback_id = next_callback_id++;
    dot_callbacks[callback_id] = callback;
    return callback_id;
}

function call_dot_callback(callback_id, output) {
    var callback = dot_callbacks[callback_id];
    delete dot_callbacks[callback_id];
    return callback.apply(this, [output]);
}

function call_dot_worker(msg) {
//    call_dot_callback(msg.callback_id, Viz(msg.dot, { format: msg.format, engine: msg.engine }));
    worker.postMessage(msg);
}

function update_dot_download(str) {
    $(document).ready(function () {
        $(".dot_download_link").each(function () {
            var link = $(this);
            link.attr("href", "data:text/plain;base64," + btoa(str));
        });
    });
}

function layout_dot(str, callback) {
    $(".dot_outdated").hide();
    $(".dot_loading").show();
    /* it's important for debugging purposes that we update the download link
       on both passes, in case something fails in graphviz on the first pass */
    update_dot_download(str);
    if (STATE.graph_position_engine() === "(none)") {
        callback("");
    } else {
        call_dot_worker({ callback_id: add_dot_callback(callback), dot: str, format: "plain", engine: STATE.graph_position_engine() });
    }
}

function display_dot(str) {
    update_dot_download(str);
    call_dot_worker({ callback_id: add_dot_callback(do_display_dot), dot: str, format: "svg", engine: STATE.graph_edge_engine() });
}


$(document).ready(function () {
    $(document).on("click", ".dot_refresh", function () {
        $("#dot").css("min-width", 0);
        do_command("graph");
    });

    $(document).on("click", ".dot_zoom_plus", function () {
        var el = $(this).closest(".mid_bar").siblings(".dot_inner_container").find(".dot");
        el.panzoom("zoom");
    });

    $(document).on("click", ".dot_zoom_minus", function () {
        var el = $(this).closest(".mid_bar").siblings(".dot_inner_container").find(".dot");
        el.panzoom("zoom", true);
    });

    $(document).on("click", ".dot_zoom_100", function () {
        var el = $(this).closest(".mid_bar").siblings(".dot_inner_container").find(".dot");
        el.panzoom("resetZoom");
    });

    $(document).on("click", ".dot_pan_centre", function () {
        var el = $(this).closest(".mid_bar").siblings(".dot_inner_container").find(".dot");
        el.panzoom("resetPan");
    });

    $(document).on("mousewheel.focal", ".dot_inner_container", function(e) {
        e.preventDefault();
        var delta = e.delta || e.originalEvent.wheelDelta;
        var zoom_out = delta ? delta < 0 : e.originalEvent.deltaY > 0;
        $(this).find(".dot").panzoom("zoom", zoom_out, {
            increment: 0.1,
            animate: false,
            focal: e
        });
    });
});

STATE.graph_position_engine.subscribe(function () {
    do_command("graph");
});

STATE.graph_edge_engine.subscribe(function () {
    do_command("graph");
});
