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
    //bool m_pinned;
};


#endif // GRAPH_CLASSES_H
