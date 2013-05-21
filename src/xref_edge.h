#ifndef XREF_EDGE_H
#define XREF_EDGE_H

#include <QString>
#include <QRect>
#include <QPoint>
#include <QGraphicsLineItem>

class xrefSceneNode;

/// Describes a connecting line between two nodes on scene
class xrefSceneEdge : public QGraphicsLineItem
{
public:
    xrefSceneEdge(xrefSceneNode * src, xrefSceneNode * dst);
    virtual ~xrefSceneEdge() {}

    void update_scene_edge_coords();

public:
    xrefSceneNode * m_src = nullptr;
    xrefSceneNode * m_dst = nullptr;
};

#endif // XREF_EDGE_H
