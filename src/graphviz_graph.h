#ifndef GRAPHVIZ_GRAPH_H
#define GRAPHVIZ_GRAPH_H

#include <QSet>
#include <QMap>
#include <QString>

#include <graphviz/gvc.h>

/// Holds information about Graphviz populated graph
class xrefGraphvizGraph
{
public:
    xrefGraphvizGraph();
    ~xrefGraphvizGraph();

    void redo_layout(const char * algo);

public:
    const char * m_last_layout = "dot";

    /// graph definition
    Agraph_t * m_graph = nullptr;
    GVC_t * m_gvc = nullptr;
    QMap<QString, Agnode_t *> m_name_to_agnode;

    /// additional attributes like rectangle on screen, pin, type etc
    //QMap<QString, xrefNode> m_node_info;
};

#endif // GRAPHVIZ_GRAPH_H
