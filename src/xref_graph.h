#ifndef XREF_GRAPH_H
#define XREF_GRAPH_H

#include <QString>
#include <QMap>
#include <QSet>

class xrefSceneEdge;
class xrefEditableNode;
class xrefSourceNode;
class xrefGraphvizGraph;

class xrefGraph {
public:
    virtual ~xrefGraph();

    void add_edges_to_scene(xrefSceneNode *caller_node);

    void load_source_nodes(const QString &fn);
    void source_to_editable_nodes();

    /// Forms a graphviz memory structure and calls graphviz layout function
    void apply_layout(const char * gv_layout_method);

    /// Forms a graphviz memory structure and calls graphviz layout function for given subset of nodes
    void apply_layout(const QSet<xrefEditableNode *> & nodes_affected,
                      const char * gv_layout_method);

public:
    QMap<QString, QList<QString>> m_app_modules;

    /// JSON data decoded is stored here read-only, this is used to fill
    /// m_editable_nodes without sizes, positions and other attributes
    QMap<QString, xrefSourceNode *> m_source_nodes;

    /// Editable items saved separately, modelled after m_source_nodes. User is
    /// allowed to do changes to this structure. This is used to fill m_scene
    QMap<QString, xrefEditableNode *> m_editable_nodes;

    QMap<QString, xrefSceneNode *> m_scene_nodes;

    /// Selection on screen
    QSet<xrefEditableNode *> m_selected_nodes;
};

#endif // XREF_GRAPH_H
