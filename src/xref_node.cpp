#include "xref_node.h"

xrefNode::xrefNode(): m_name()
{
}

xrefNode::xrefNode(const QString &name, Agnode_t *gv_node)
    : m_name(name), m_graphviz_node(gv_node)
{
}
