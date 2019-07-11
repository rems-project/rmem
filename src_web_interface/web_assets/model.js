/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Jon French, University of Cambridge   2017                         */
/*  Copyright Shaked Flur, University of Cambridge  2017                         */
/*  Copyright Peter Sewell, University of Cambridge 2017                         */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  It is part of the rmem tool, distributed under the 2-clause BSD licence in   */
/*  LICENCE.txt.                                                                 */
/*                                                                               */
/*===============================================================================*/

function StateModel () {
    this.test_name = ko.observable("(no test loaded)");
    this.isa_name = ko.observable("no ISA loaded");
    this.test_type = ko.observable("");
    this.model_name = ko.observable("");
    this.version = ko.observable("");
    this.build_date = ko.observable("");
    this.prompt = ko.observable("");
    this.console_lines = ko.observable("");
    this.state_lines = ko.observable("");
    this.trace_lines = ko.observable("");
    this.trace_available_transitions = ko.observable("");
    this.graph_html = ko.observable("");
    this.graph_position_engine = ko.observable("dot");
    this.graph_edge_engine = ko.observable("neato");
    this.sources = ko.observableArray();
    this.litmus_library = ko.observableArray();
    this.elf_library = ko.observableArray();
    this.tree_speculation = ko.observable("allow");
    this.embedding = ko.observable("interpreter");
    this.force_sc = ko.observable("force_sc_false");
    this.relaxed_fetch = ko.observable("relaxed_fetch");
    this.fetch_flat_idc = ko.observable("fetch_flat_idc");
    this.fetch_flat_dic = ko.observable("fetch_flat_dic");
    this.sequential_fetch = ko.observable("sequential_fetch_false");
}

var STATE = new StateModel;

var components = {
    "console": "Console",
    "state": "State",
    "graph": "Graph",
    "sources": "Sources",
    "trace": "Trace",
    "help": "Help"
};
Object.keys(components).forEach(function (c) {
    ko.components.register(c, {
        viewModel: { instance: STATE },
        template: { element: c + "_template" }
    });
});

ko.bindingHandlers.switcher = {
    init: function (element, value_accessor, all_bindings) {
        var el = $(element);
        var value = ko.unwrap(value_accessor());
        for (var id in components) {
            var name = components[id];
            var option = $('<option value="' + id + '"' + ((id === value) ? ' selected="selected"' : '') + '>' + name + '</option>');
            el.append(option);
        }
        el.on("change", function (e) {
            var that = $(this);
            var previous_pane = that.closest(".pane");
            switch_panes(previous_pane, that.val());
        });
    }
};

/* Based on example from Knockout documentation */
var template_from_url_loader = {
    loadTemplate: function(name, template_config, callback) {
        if (template_config.fromUrl) {
            $.get(template_config.fromUrl, function(s) {
                var doc = $(s);
                doc.find("a").attr("target", "_blank");
                callback(doc.toArray());
            });
        } else {
            // Unrecognized config format. Let another loader handle it.
            callback(null);
        }
    }
};

ko.components.loaders.unshift(template_from_url_loader);

ko.components.register("help_loader", {
    template: { fromUrl: "help.html" }
});

ko.tasks.schedule_promise = function () {
    return new Promise(function (resolve, reject) {
        ko.tasks.schedule(resolve);
    });
};

$(".ko").each(function () {
    ko.applyBindings(STATE, this);
});
