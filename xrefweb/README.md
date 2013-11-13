# XrefGUI Web Server

The XrefGUI application is a separate Erlang node with a web server, which
  connects to your running application, extracts XREF information from loaded
  modules, and offers you visualisation options.

## How to use

* Configure node name and Erlang cookie for your running Erlang node;
* Start the xrefweb node using `make run`
* Go with your web browser to `http://localhost:12000`

The xrefgui application should be able to connect to your node, figure out what's
running and will start a webserver for you.