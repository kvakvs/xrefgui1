#include "xref_edge.h"
#include "xref_node.h"

xrefEditableEdge::xrefEditableEdge(xrefEditableNode * src, xrefEditableNode * dst)
    : QGraphicsLineItem(), m_src(src), m_dst(dst)
{
    update_edge_coords();
}

void xrefEditableEdge::update_edge_coords()
{
    auto p1 = m_src->get_attach_point_for_edge();
    auto p2 = m_dst->get_attach_point_for_edge();
    QLineF new_line(p1, p2);
    if (line() != new_line) {
        setLine(new_line);
    }
}
