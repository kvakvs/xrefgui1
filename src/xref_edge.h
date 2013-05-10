#ifndef XREF_EDGE_H
#define XREF_EDGE_H

#include <QString>
#include <QRect>
#include <QPoint>
#include <QGraphicsLineItem>

class xrefEditableNode;

/// Describes a connecting line between two nodes on scene
class xrefEditableEdge : public QGraphicsLineItem
{
public:
    xrefEditableEdge(xrefEditableNode * src, xrefEditableNode * dst);
    virtual ~xrefEditableEdge() {}

    void update_edge_coords();

public:
    xrefEditableNode * m_src = nullptr;
    xrefEditableNode * m_dst = nullptr;
};

#endif // XREF_EDGE_H
