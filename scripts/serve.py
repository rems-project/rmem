#===============================================================================#
#                                                                               #
#                rmem executable model                                          #
#                =====================                                          #
#                                                                               #
#  This file is:                                                                #
#                                                                               #
#  Copyright Jon French, University of Cambridge  2017                          #
#  Copyright Shaked Flur, University of Cambridge 2017                          #
#                                                                               #
#  All rights reserved.                                                         #
#                                                                               #
#  It is part of the rmem tool, distributed under the 2-clause BSD licence in   #
#  LICENCE.txt.                                                                 #
#                                                                               #
#===============================================================================#

#!/usr/bin/env python

from __future__ import print_function

import argparse
import sys

PY3 = (sys.version_info > (3, 0))

if PY3:
    import http.server
    import socketserver
    base = http.server.SimpleHTTPRequestHandler
    server_module = http.server
    socket_module = socketserver
else:
    import BaseHTTPServer
    import SimpleHTTPServer
    import SocketServer
    base = SimpleHTTPServer.SimpleHTTPRequestHandler
    server_module = BaseHTTPServer
    socket_module = SocketServer

class RequestHandler(base, object):
    protocol_version = "HTTP/1.1"
    def end_headers(self):
        self.send_header("Cache-Control", "max-age=0, no-cache, no-store, must-revalidate")
        self.send_header("Pragma", "no-cache")
        self.send_header("Expires", "Thu, 01 Jan 1970 00:00:00 UTC")
        super(RequestHandler, self).end_headers()

class HTTPServer(socket_module.ThreadingMixIn, server_module.HTTPServer):
    pass

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("port", action="store", default=8000, type=int, nargs="?")
    args = parser.parse_args()
    httpd = HTTPServer(("localhost", args.port), RequestHandler)
    sa = httpd.socket.getsockname()
    print("Serving HTTP on", sa[0], "port", sa[1], "...")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nKeyboard interrupt received, exiting.")
        httpd.server_close()
        sys.exit(0)
