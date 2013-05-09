#ifndef GRAPH_CLASSES_H
#define GRAPH_CLASSES_H

#include <QSet>
#include <QString>
#include <QRect>
#include <QPoint>
#include <QGraphicsRectItem>

//class Agnode_t;

/// Describes a node information as loaded from JSON
class xrefSourceNode
{
public:
    QString m_name;
    QSet<QString> m_callees;
};

/// Describes a visible node on screen.
class xrefEditableNode: public QGraphicsRectItem
{
public:
    xrefEditableNode();
    xrefEditableNode(const QString & name);

    QString m_name;
    //QRectF m_rect;

    bool m_pinned; // do not move node when changing layout
    bool m_draw_out_edges = false;
    bool m_draw_in_edges = false;

    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                       QWidget *widget);
};

#endif // GRAPH_CLASSES_H
