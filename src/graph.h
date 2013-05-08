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
    xrefNode(const QString & name);

    QString m_name;
//    QSet<QString> m_edges_out;
//    QSet<QString> m_edges_in; // probably this is extraneous and remove it later
    QRect m_rect;
};


#endif // GRAPH_CLASSES_H
