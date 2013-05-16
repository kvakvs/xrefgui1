#include "xref_edge.h"
#include "xref_node.h"

xrefSceneEdge::xrefSceneEdge(xrefSceneNode *src, xrefSceneNode *dst)
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
