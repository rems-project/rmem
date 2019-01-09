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

(function(mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
        mod(require("../../lib/codemirror"));
    else if (typeof define == "function" && define.amd) // AMD
        define(["../../lib/codemirror"], mod);
    else // Plain browser env
        mod(CodeMirror);
})(function(CodeMirror) {
    "use strict";

    var start_comment = { regex: /\(\*/, token: "comment", push: "comment" };
    var info_rules = [
        { regex: /([a-zA-Z][a-zA-Z0-9_/.-]*)(\s*)(=)(\s*)(.*)/, token: ["variable", null, "bracket", null, "string"], next: "info", eol: true },
        { regex: /{/, token: "bracket", next: "init" },
        { regex: /.*/, token: "error", next: "info", eol: true }
    ];

    var stock = {
        name: /(\$?[a-zA-Z_][a-zA-Z0-9_/.-]*\b)/.source,
        num: /(#?(?:0[xX])?[0-9a-fA-F]+\b)/.source,
        s: /(\s+)/.source,
        nat: /(#?\d+\b)/.source,
        mips_reg: /(\$(?:[0-9]?[0-9]|pc|hi|lo|res)\b)/.source,
        ppc_reg: /((?:cr|[Rr]|FPR|fpr)[0-9]?[0-9]\b)/.source,
        arm_reg: /((?:(?:[RrXxWw][0-9]?[0-9])|[wx]?zr|[WX]?ZR|[wx]?pc|[WX]?PC|[wx]?sp|[WX]?SP|[wx]?lr|[WX]?LR)\b)/.source,
        symb_reg: /(%[a-zA-Z_][a-zA-Z0-9_/.-]*\b)/.source,
        x86_reg: /(%?(?:(?:[RrEe]?(?:(?:[AaBbCcDd][XxHhLl])|(?:[SsDd][Ii])|(?:[SsCcDdEeFfGg][Ss])|flags|FLAGS|ip|IP))|(?:[Rr](?:[0-9]|1[0-5])[DdWwLl]?)))/.source,
        reg: /((?:(?:(?:[RrXxWw][0-9]?[0-9])|[wx]?zr|[WX]?ZR|[wx]?pc|[WX]?PC|[wx]?sp|[WX]?SP|[wx]?lr|[WX]?LR)|(?:(?:cr|[Rr]|FPR|fpr)[0-9]?[0-9])|(?:\$(?:[0-9]?[0-9]|pc|hi|lo|res))|(?:[a-z])|(?:%[a-zA-Z_][a-zA-Z0-9_\/.-]*)|(?:%?(?:(?:[RrEe]?(?:(?:[AaBbCcDd][XxHhLl])|(?:[SsDd][Ii])|(?:[SsCcDdEeFfGg][Ss])|flags|FLAGS|ip|IP))|(?:[Rr](?:[0-9]|1[0-5])[DdWwLl]?))))\b)/.source,
        quantifier: /((?:final|forall|(?:~\s*)?exists|cases|[Oo]bserved|locations|filter)\b)/.source,
        _quantifier: /(?:(?:final|forall|(?:~\s*)?exists|cases|[Oo]bserved|locations|filter)\b)/.source,
        prop_op: /((?:not\b|~|\/\\|\\\/|=>))/.source,
        mnemonic: /([A-Za-z]+(?:\.(?:[A-Za-z]+)?)?(?:\s+(?:SY|OSH|NSH|ISH|(?:OSH|NSH|ISH)?(?:LD|ST)))?\b)/.source,
        shift: /(LSL|LSR|ASR|ROR|SXTW|UXTW\b)/.source,
    };

    CodeMirror.defineSimpleMode("litmus", {
        start: [
            { regex: /\s+/, token: null, sol: true },
            { regex: /(AArch64|ARM|LitmusAArch64|MIPS|PPC|X86|RISCV)(\s+)/, token: ["keyword", null], next: "name", sol: true },

            { regex: /(.+)(\s)/, token: ["error", null], next: "name" },
            { regex: /.*/, token: "error", eol: true }
        ],

        name: [
            { regex: /[0-9a-zA-Z_/.+[\]\s-]+/, token: "variable", next: "maybe_doc", eol: true },

            { regex: /.*/, token: "error", next: "maybe_doc", eol: true },
        ],

        maybe_doc: [
            { regex: /"[^"]*"/, token: "string-2", next: "info", eol: true }
        ].concat(info_rules),

        info: info_rules,

        init: [
            { regex: /\s+/, token: null },
            { regex: /;/, token: "bracket" },
            { regex: /}/, token: "bracket", next: "procs" },
            // [type] x [= (1|y)];
            { regex: new RegExp(format("(?:{name}{s})?" + // [type ]
                                       "{name}" + // name
                                       "(?:{s}?(=){s}?(\\*)?{s}?(?:{name}|{num}))?" + // [[ ]=[ ](1|x)]
                                       "\\b{s}?(;)?", stock)), // [ ];
              token: ["variable-3", null, "variable", null, "bracket", null, "keyword", null, "variable", "number", null, "bracket"] },
            { regex: new RegExp(format("{nat}{s}?(:){s}?{reg}" + // 1[ ]:[ ]R0
                                       "(?:{s}?(=){s}?(\\*)?{s}?(?:{name}|{num}))?" + // [[ ]=[ ](1|x)]
                                       "\\b{s}?(;)?", stock)), // [ ];
              token: ["number", null, "bracket", null, "variable-2", null, "bracket", null, "keyword", null,  "variable", "number", null, "bracket"] },

            { regex: /(.*?)(;)/, token: ["error", "bracket"] },
            { regex: /.*/, token: "error", eol: true }
        ],

        procs: [
            start_comment,
            { regex: /\s+/, token: null },
            { regex: /([Pp]\d+)(\s+)(\|)/, token: ["tag", null, "bracket"] },
            { regex: /([Pp]\d+)(\s+)(;)/, token: ["tag", null, "bracket"], next: "prog" },

            { regex: /(.*?)(\|)/, token: ["error", "bracket"] },
            { regex: /(.*?)(;)/, token: ["error", "bracket"], next: "prog" },
            { regex: /.*/, token: "error", eol: true }
        ],

        prog: [
            start_comment,
            { regex: /\s+/, token: null },
            { regex: new RegExp(format("(?={_quantifier}\\b)", stock)), next: "constraints" },

            { regex: new RegExp(format("{name}{s}?(:)", stock)), token: ["def", null, "bracket"] },
            { regex: new RegExp(format("{mnemonic}{s}", stock)), token: ["atom", null], next: "prog_operands" },

            { regex: /(.*?)(\|)/, token: ["error", "bracket"] },
            { regex: /(.*?)(;)/, token: ["error", "bracket"] },
            { regex: /.*/, token: "error", eol: true }
        ],

        prog_operands: [
            start_comment,
            { regex: /\s+/, token: null },
            { regex: /,/, token: "bracket" },
            { regex: /;|\|/, token: "bracket", next: "prog" },

            { regex: new RegExp(format("(\\[){s}?{reg}{s}?(,){s}?{reg}{s}?(,){shift}{s}?{num}?{s}?(\\])", stock)), token: ["keyword", null, "variable-2", null, "bracket", null, "variable-2", null, "bracket", "keyword", null, "number", null, "keyword"] },
            { regex: new RegExp(format("(\\[){s}?{reg}{s}?(,){s}?{reg}{s}?(\\])", stock)), token: ["keyword", null, "variable-2", null, "bracket", null, "variable-2", null, "keyword"] },
            { regex: new RegExp(format("(?:(\\[){s}?)?{reg}(?:{s}?(\\]))?", stock)), token: ["keyword", null, "variable-2", null, "keyword"] },
            { regex: new RegExp(format("{shift}{s}?{num}", stock)), token: ["keyword", null, "number"] },
            { regex: new RegExp(format("{num}?(\\(){s}?{reg}{s}?(\\))", stock)), token: ["number", "bracket", null, "variable-2", null, "bracket"] },
            { regex: new RegExp(format("{num}", stock)), token: "number" },
            { regex: new RegExp(format("(=)?{name}", stock)), token: ["keyword", "variable"] },

            { regex: /(.*?)(\|)/, token: ["error", "bracket"], next: "prog" },
            { regex: /(.*?)(;)/, token: ["error", "bracket"], next: "prog" },
            { regex: /(.*?)(\])/, token: ["error", "keyword"] },
            { regex: /(.*?)(,)/, token: ["error", "bracket"] },
            { regex: /.*/, token: "error", eol: true, next: "prog" }
        ],

        constraints: [
            start_comment,
            { regex: /\s+/, token: null },
            { regex: /[()]/, token: "bracket" },
            { regex: new RegExp(format("{quantifier}\\b", stock)), token: "keyword" },
            { regex: /(true|false)\b/, token: ["atom", null] },
            { regex: new RegExp(format("{prop_op}", stock)), token: "keyword" },
            // [type] x [= (1|y)];
            { regex: new RegExp(format("(?:{name}{s})?" + // [type ]
                                       "{name}" + // name
                                       "(?:{s}?(=){s}?(\\*)?{s}?(?:{name}|{num}))?" + // [[ ]=[ ](1|x)]
                                       "\\b", stock)), // [ ];
              token: ["variable-3", null, "variable", null, "bracket", null, "keyword", null, "variable", "number"] },
            { regex: new RegExp(format("{nat}{s}?(:){s}?{reg}" + // 1[ ]:[ ]R0
                                       "(?:{s}?(=){s}?(\\*)?{s}?(?:{name}|{num}))?" + // [[ ]=[ ](1|x)]
                                       "\\b", stock)), // [ ];
              token: ["number", null, "bracket", null, "variable-2", null, "bracket", null, "keyword", null,  "variable", "number"] },

            { regex: /.*/, token: "error", eol: true }
        ],

        comment: [
            { regex: /.*?\*\)/, token: "comment", pop: true },
            { regex: /.*/, token: "comment" }
        ],

        meta: {
            blockCommentStart: "(*",
            blockCommentEnd: "*)"
        }
    });

});
