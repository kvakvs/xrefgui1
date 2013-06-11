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

1.  Compile your application, we need `.beam` files!
1.  Go to erl_csi folder, and create config file using `config.example` as template, add
    path to your application directory (where all application dirs are stored, this can be
    your application root,
    or `apps/` subdirectory in your project if you have multiple applications, of `lib/`
    really depends on your project structure). Add all application names you're
    interested in.
1.  Run the attached script using config filename as parameter, it will crash if you have
    a problem (like duplicate module
    names) or produce `input.json` on success. It contains call graph information as well
    as module/application relations.
1.  Copy `input.json` to the xrefgui starting directory
1.  Run xrefgui.

# input.json format

```
{ "connections": {"modname3": ["modname1", "modname2", ...]}
, "applications": {"appname1": ["app1mod1", "app1mod2", ...]}
, "calls", {"m1:f1/arity": ["m2:f2/arity", "m3:f3/arity"...]}
}
```

*   *connections* defines module detail level call graph.
*   *applications* defines how modules group in relation to applications.
*   *calls* defined function detail level call graph.
