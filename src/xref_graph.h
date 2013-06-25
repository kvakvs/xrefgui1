#ifndef XREF_GRAPH_H
#define XREF_GRAPH_H

#include <QString>
#include <QMap>
#include <QSet>
#include <QTransform>
#include <QBrush>

class xrefSceneNode_Module;
class xrefSceneEdge;
class xrefEditableNode;
class xrefSourceNode;
class xrefGraphvizGraph;
class xrefCApp;
class xrefCMod;
class xrefCFun;

class xrefGraph {
public:
    virtual ~xrefGraph();

    //------ VIEW support ------
    bool view_new(const QString & name);
    bool view_select(const QString & name);
    bool view_delete(const QString & name);
    QList<QString> view_get_names();
    const QString & view_get_current();
    //--------------------------

    void add_edges_to_scene(xrefSceneNode_Module *caller_node);

    void load_source_nodes(const QString &fn);

    /// Copies names and edges from source nodes to editable nodes
    void source_to_editable_nodes();

    /// Copies names and edges from editable nodes to scene
    void recreate_scene_from_editable(const QSet<QString> & node_names);

    /// Forms a graphviz memory structure and calls graphviz layout function
    void apply_layout(const char * gv_layout_method);

    /// Forms a graphviz memory structure and calls graphviz layout function for given subset of nodes
    void apply_layout(const QSet<xrefSceneNode_Module *> & nodes_affected,
                      const char * gv_layout_method);

    void clear_editable();
    void transform(const QTransform & tr);

    /// Saves coords from scene to editable nodes
    void save_scene_to_editable();

    xrefCFun * code_get_or_create_fun(const QString & name);
    xrefCFun * code_get_fun(const QString & name);
    xrefCMod * code_get_mod(const QString & name);
    xrefCApp * code_get_app(const QString & name);
    bool code_parse_fun_name(const QString &full_name, QString &out_app,
                             QString &out_mod, QString &out_fun);
    xrefCApp * code_get_app_by_module_name(const QString & mod);

private:
    void load_mod_calls(const QJsonObject & jroot);
    void load_apps(const QJsonObject & jroot);
    void load_fun_calls(const QJsonObject & jroot);

    /// Selects item background color to distinguish per application
    static QBrush choose_brush(xrefEditableNode * ed_node,
                               xrefSceneNode_Module * scene_node);

public:
    QMap<QString, QList<QString>> m_app_modules;

    /// JSON data decoded is stored here read-only, this is used to fill
    /// m_editable_nodes without sizes, positions and other attributes
    QMap<QString, xrefSourceNode *> m_source_nodes;

    QMap<QString, xrefSceneNode_Module *> m_scene_nodes;

    /// Selection on screen
    QSet<xrefEditableNode *> m_selected_nodes;

    /// Callgraph edges for functions
    QMap<xrefCFun *, xrefCFun *> m_fun_calls;

private:
    /// Code structure: Applications
    QMap<QString, xrefCApp *> m_code_apps;
    /// Code structure: Modules
    QMap<QString, xrefCMod *> m_code_modules;
    /// Code structure: Functions
    QMap<QString, xrefCFun *> m_code_functions;

    class View {
    public:
        explicit View(const QString n): m_editable_nodes(), m_name(n) {
        }
        ~View();

        void clear();

        /// Editable items saved separately, modelled after m_source_nodes. User is
        /// allowed to do changes to this structure. This is used to fill m_scene
        QMap<QString, xrefEditableNode *> m_editable_nodes;
        QString m_name;
    };

    /// Collection of named editable views of the input graph (with coords, colors etc)
    QMap<QString, View *> m_views;

    /// Current selected view
    View * m_view = nullptr;
};

#endif // XREF_GRAPH_H
