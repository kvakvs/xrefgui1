#ifndef XREF_NODE_H
#define XREF_NODE_H

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

class xrefEditableEdge;

/// Describes a visible node on screen.
class xrefEditableNode: public QGraphicsRectItem
{
public:
    //xrefEditableNode();
    xrefEditableNode(const QString & name);
    virtual ~xrefEditableNode() {}

    QPointF get_attach_point_for_edge();

    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                       QWidget *widget);
    virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);
    virtual void mouseMoveEvent(QGraphicsSceneMouseEvent *event);

public:
    QString m_name;
    QList<xrefEditableEdge *> m_linked_edges;

    bool m_pinned; // do not move node when changing layout
    bool m_draw_out_edges = false;
    bool m_draw_in_edges = false;
};

#endif // XREF_NODE_H
