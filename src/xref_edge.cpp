#include "xref_edge.h"
#include "xref_node.h"
#include "render_things.h"

xrefSceneEdge::xrefSceneEdge(xrefSceneNode_Module *src, xrefSceneNode_Module *dst)
    : QGraphicsLineItem(), m_src(src), m_dst(dst)
{
    update_scene_edge_coords();
}

void xrefSceneEdge::update_scene_edge_coords()
{
    if (! m_src) return;
    if (! m_dst) return;
    auto p1 = m_src->get_attach_point_for_edge();
    auto p2 = m_dst->get_attach_point_for_edge();
    QLineF new_line(p1, p2);
    if (line() != new_line) {
        setLine(new_line);
    }
}

void xrefSceneEdge::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    QLineF l = line();
    DrawThings::arrow(*painter, l.p1(), l.p2());
}
