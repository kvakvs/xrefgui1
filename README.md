# Erlang xref GUI

In few words - this tool is a QT GUI designed to display output of xref Erlang
tool parsing your project, and display different views of the graph, show and
hide modules, functions, dependencies, callers/callees, comment and manually
arrange nodes.

Key concepts:

*   Any edits you do are saved separately from input and never lost.
*   You can reread input repeatedly, and this will add new or remove missing
    nodes, so that you can continuously use the same view on an evolving code.
*   You can have many views of the same graph for your different needs.

# Building

1.  Download QT5 for your operating system from [QT Project website](http://qt-project.org)
1.  Install QT (by setting runnable attribute on Linux installer and running it
    with administration rights). `DO NOT` run IDE from installer, as it will
    create configs in your home directory with root ownership, run IDE from
    your desktop menu instead.
1.  Make sure you have installed graphviz development library and also libGL
    development library may be required at build time on Linux, even if you
    don't have OpenGL drivers currently running, it will use MESA software GL
    driver automatically.
1.  Open *.pro file in QT, and hit BUILD, that should do it.

# Using

1.  Prepare data using this tool: git://github.com/lehoff/erl_csi.git
    (it just wraps around Erlang xref library)
1.  Recode data to JSON using ???? (hello JD!)
1.  Put input.json in the program starting directory
1.  Run the GUI.
