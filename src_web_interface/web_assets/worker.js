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

importScripts('lib/viz.js');
onmessage = function (e) {
    postMessage({ callback_id: e.data.callback_id, output: Viz(e.data.dot, { format: e.data.format, engine: e.data.engine })});
}
