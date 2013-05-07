#include "graph.h"

xrefNode::xrefNode(): m_name(), m_edges_out(), m_edges_in()
{
}

xrefNode::xrefNode(const QString &name)
    : m_name(name), m_edges_out(), m_edges_in()
{
}
