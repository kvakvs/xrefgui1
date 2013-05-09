#ifndef GRAPH_CLASSES_H
#define GRAPH_CLASSES_H

#include <QSet>
#include <QString>
#include <QRect>
#include <QPoint>

class Agnode_t;

/// Describes a node information as loaded from JSON
class xrefSourceNode
{
public:
    QString m_name;
    QSet<QString> m_callees;
};

/// Describes a visible node on screen.
class xrefEditableNode
{
public:
    xrefEditableNode();
    xrefEditableNode(const QString & name, Agnode_t * gv_node);

    QString m_name;
    QRectF m_rect;

    Agnode_t * m_graphviz_node = nullptr;
    bool m_pinned; // do not move node when changing layout
    bool m_draw_out_edges = false;
    bool m_draw_in_edges = false;
};

#endif // GRAPH_CLASSES_H
