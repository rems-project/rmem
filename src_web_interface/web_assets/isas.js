/*===============================================================================*/
/*                                                                               */
/*                rmem executable model                                          */
/*                =====================                                          */
/*                                                                               */
/*  This file is:                                                                */
/*                                                                               */
/*  Copyright Shaked Flur, University of Cambridge 2017                          */
/*  Copyright Jon French, University of Cambridge  2017                          */
/*                                                                               */
/*  All rights reserved.                                                         */
/*                                                                               */
/*  The rmem tool is distributed under the 2-clause BSD license in LICENCE.txt.  */
/*  For author information see README.md.                                        */
/*                                                                               */
/*===============================================================================*/

function read_filename (basename) {
    var request = $.ajax(basename, {
        /*
          NOTE: This (async: false) is deprecated for fairly obvious reasons, it
          freezes the page while the request happens and therefore no one likes it.

          TODO: refactor so that we can actually do this asynchronously. You're
          allowed to do it sync in web worker threads, so possibly this will
          fall out of the goal to eventually move the Ocaml to a separate thread
        */
        async: false,
        dataType: "text",
        processData: false
    });
    return request.responseText;
}
