#ifndef XREF_EDGE_H
#define XREF_EDGE_H

#include <QString>
#include <QRect>
#include <QPoint>
#include <QGraphicsLineItem>

class xrefSceneNode_Module;
static const int xrefNodeType_Call     = 100002;

/// Describes a connecting line between two nodes on scene
class xrefSceneEdge : public QGraphicsLineItem
{
public:
    xrefSceneEdge(xrefSceneNode_Module * src, xrefSceneNode_Module * dst);
    virtual ~xrefSceneEdge() {}

    void update_scene_edge_coords();

public:
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                       QWidget *widget);
    virtual int type() const { return xrefNodeType_Call; }

public:
    xrefSceneNode_Module * m_src = nullptr;
    xrefSceneNode_Module * m_dst = nullptr;
};

#endif // XREF_EDGE_H
