#ifndef XREF_NODE_H
#define XREF_NODE_H

#include <QSet>
#include <QString>
#include <QRect>
#include <QPoint>
#include <QGraphicsRectItem>

/// Describes a node information as loaded from JSON
class xrefSourceNode
{
public:
    QString m_name;
    QString m_app_name;
    QSet<QString> m_callees;
};


class xrefSceneEdge;


/// Describes node as part of editable set
class xrefEditableNode
{
public:
    //xrefEditableNode();
    xrefEditableNode(const QString & name);
    virtual ~xrefEditableNode() {}

public:
    xrefSourceNode * m_src_node = nullptr;
    /// Node name and displayed text
    QString m_name;
    /// Group or app name node belongs to
    QString m_app_name;

//    struct {
//        bool show = true;
//        bool edges_out = true;
//        bool edges_in = false;
//    } m_editor_flags;

    QPointF m_position;
    QSizeF  m_scene_size;
//    /// do not move node when changing layout
//    bool m_pinned;
//    bool m_layout_use_out_edges = false;
//    bool m_layout_use_in_edges = false;
//    bool m_show_out_edges = false;
//    bool m_show_in_edges = false;
};

static const int xrefNodeType_Module   = 100000;
static const int xrefNodeType_Function = 100001;

class xrefSceneNode_Module: public QGraphicsRectItem
{
public:
    xrefSceneNode_Module(xrefEditableNode * node);
    virtual ~xrefSceneNode_Module() {}

    QPointF get_attach_point_for_edge();
    void set_rect_update_edges(const QRectF &rect);

    /// Searches linked edges for specific direction
    bool has_edge(xrefSceneNode_Module * nfrom, xrefSceneNode_Module * nto);

public:
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                       QWidget *widget);
    virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);
    virtual void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
    virtual int type() const { return xrefNodeType_Module; }

public:
    QString m_app_name;
    QString m_name;
    xrefEditableNode * m_node = nullptr;
    QList<xrefSceneEdge *> m_linked_edges;
};

#endif // XREF_NODE_H
