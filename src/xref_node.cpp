#include <graphviz/gvc.h>

#include "xref_node.h"

xrefEditableNode::xrefEditableNode(): m_name()
{
}

xrefEditableNode::xrefEditableNode(const QString &name, Agnode_t *gv_node)
    : m_name(name), m_graphviz_node(gv_node)
{
}
