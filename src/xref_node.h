#ifndef GRAPH_CLASSES_H
#define GRAPH_CLASSES_H

#include <QSet>
#include <QString>
#include <QRect>
#include <QPoint>

#include <graphviz/gvc.h>

class xrefNode
{
public:
    xrefNode();
    xrefNode(const QString & name, Agnode_t * gv_node);

    Agnode_t * m_graphviz_node = nullptr;

    QString m_name;
    QRectF m_rect;
    bool m_pinned; // do not move node when changing layout
    bool m_draw_out_edges = false;
    bool m_draw_in_edges = false;
    QSet<QString> m_groups;
};


#endif // GRAPH_CLASSES_H
