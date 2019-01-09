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

function get_cookie (name) {
    var prefix = name + "=";
    var cookies = document.cookie.split(";");
    for(var i = 0; i < cookies.length; i++) {
        var cookie = $.trim(cookies[i]);
        if (cookie.startsWith(prefix)) {
            return decodeURIComponent(cookie.substring(prefix.length, cookie.length));
        }
    }
    return null;
}

function set_cookie (name, value) {
    document.cookie = name + "=" + encodeURIComponent(value);
}

function delete_cookie (name) {
    document.cookie = name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC";
}

function error_dialog (msg) {
    var div = $('<div class="popup_dialog"><span class="ui-icon ui-icon-alert" style="margin: 5px"></span><div class="msg">' + msg + '</div></div>');
    var callback = function () {
        div.dialog("destroy");
        div.remove();
    };
    div.dialog({
        title: "Error",
        resizable: false,
        modal: true,
        height: "auto",
        minHeight: 50,
        maxHeight: window.innerHeight - 100,
        width: "auto",
        minWidth: 150,
        maxWidth: window.innerWidth - 100,
        buttons: {
            "OK": callback
        },
        close: callback
    });
}

function confirm_dialog (msg, callback) {
    var div = $('<div class="popup_dialog"><span class="ui-icon ui-icon-info" style="margin: 5px"></span><div class="msg">' + msg + '</div></div>');
    var closer = function () {
        if (div.dialog("instance") !== undefined) {
            div.dialog("destroy");
            div.remove();
        }
    };
    div.dialog({
        title: "Confirm",
        resizable: false,
        modal: true,
        height: "auto",
        minHeight: 50,
        maxHeight: window.innerHeight - 100,
        width: "auto",
        minWidth: 150,
        maxWidth: window.innerWidth - 100,
        buttons: {
            "OK": function () {
                closer();
                callback();
            },
            "Cancel": closer
        },
        close: closer
    });
}

function message_dialog (msg, callback) {
    var div = $('<div class="popup_dialog"><span class="ui-icon ui-icon-info" style="margin: 5px"></span><div class="msg">' + msg + '</div></div>');
    var closer = function () {
        if (div.dialog("instance") !== undefined) {
            div.dialog("destroy");
            div.remove();
        }
    };
    div.dialog({
        resizable: false,
        modal: true,
        height: "auto",
        minHeight: 50,
        maxHeight: window.innerHeight - 100,
        width: "auto",
        minWidth: 150,
        maxWidth: window.innerWidth - 100,
        buttons: {
            "OK": function () {
                closer();
                callback();
            }
        },
        close: closer
    });
}


function processing_dialog (msg) {
    var div = $('<div class="popup_dialog"><img src="images/hourglass.gif" width="48" height="48" alt="">&nbsp;<div class="msg">' + msg + '</div></div>');
    var closer = function () {
        if (div.dialog("instance") !== undefined) {
            div.dialog("destroy");
            div.remove();
        }
    };
    div.dialog({
        dialogClass: "no_titlebar",
        height: "auto",
        minHeight: 50,
        maxHeight: window.innerHeight - 100,
        width: "auto",
        minWidth: 150,
        maxWidth: window.innerWidth - 100,
        resizable: false,
        modal: true,
    });
    return closer;
}

function transient_dialog (msg, timeout) {
    var div = $('<div class="popup_dialog"><span class="ui-icon ui-icon-check" style="margin: 5px"></span><div class="msg" style="margin-left: 25px">' + msg + '</div><div class="clear"></div></div>');
    var closer = function () {
        if (div.dialog("instance") !== undefined) {
            div.dialog("destroy");
            div.remove();
        }
    };
    div.dialog({
        dialogClass: "no_titlebar",
        height: "auto",
        minHeight: 70,
        maxHeight: window.innerHeight - 100,
        width: "auto",
        minWidth: 150,
        maxWidth: window.innerWidth - 100,
        resizable: false,
        modal: true,
    });
    window.setTimeout(closer, (timeout ? timeout : 1500));

}

function clear_this_errors () {
    $(this).find("*").removeClass("ui-state-error");
}

function escape_html (text) {
    "use strict";
    return text.replace(/[\"&<>]/g, function (a) {
        return { '"': "&quot;", "&": "&amp;", "<": "&lt;", ">": "&gt;" }[a];
    });
}

function jqxhr_to_exc (f) {
    return function (exc_or_arr) {
        if (Array.isArray(exc_or_arr)) {
            var text_status = exc_or_arr[1];
            var error_thrown = exc_or_arr[2];
            return f(new Error(text_status + (error_thrown ? " - " + error_thrown : "")));
        } else {
            return f(exc_or_arr);
        }
    };
}

function jqxhr_to_promise (jqxhr) {
    return new Promise(function (resolve, reject) {
        jqxhr.done(function (data, text_status, jqxhr) {
            resolve(data);
        });
        jqxhr.fail(function (jqxhr, text_status, error_thrown) {
            reject([jqxhr, text_status, error_thrown]);
        });
    });
}
