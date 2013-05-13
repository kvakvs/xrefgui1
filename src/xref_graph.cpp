#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include "xref_node.h"
#include "xref_edge.h"
#include "xref_graph.h"
#include "graphviz_graph.h"
#include "main_window.h"

xrefGraph::~xrefGraph()
{
    foreach(xrefSourceNode * n, m_source_nodes) {
        delete n;
    }
    m_source_nodes.clear();
    foreach(xrefEditableNode * n, m_editable_nodes) {
        delete n;
    }
    m_editable_nodes.clear();
}

void xrefGraph::add_edges_to_scene(xrefEditableNode * caller_node)
{
    auto main = MainWindow::m_singleton;

    for(auto callee: caller_node->m_src_node->m_callees) {
        xrefEditableNode * callee_node = m_editable_nodes[callee];

        auto edge = new xrefEditableEdge(caller_node, callee_node);
        edge->setZValue(10);
        caller_node->m_linked_edges.append(edge);
        callee_node->m_linked_edges.append(edge);
        main->m_scene->addItem(edge);
    }
}

void xrefGraph::load_source_nodes(const QString &fn)
{
    QFile file(fn);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        auto bytes = file.readAll();
        auto jdoc = QJsonDocument::fromJson(bytes);
        auto jroot = jdoc.object();

        // input -> {connections: {mod1: ['mod2', 'mod3']}}
        auto connections = jroot.value("connections").toObject();
        for (auto n1iter = connections.begin(); n1iter != connections.end(); ++n1iter)
        {
            auto node1_name = n1iter.key();
            auto src_node = new xrefSourceNode();
            src_node->m_name = node1_name;

            auto value_list = n1iter.value().toArray();
            for (auto n2iter = value_list.begin(); n2iter != value_list.end(); ++n2iter)
            {
                auto callee_name = (* n2iter).toString();
                src_node->m_callees.insert(callee_name);
            } // for callee list

            m_source_nodes[node1_name] = src_node;
        } // for json keys
    } // if file
}

void xrefGraph::source_to_editable_nodes()
{
    auto main = MainWindow::m_singleton;

    // CLEAR USER EDITS! do not call this more than once after first import
    //for(auto n = m_editable_nodes.begin(); n != m_editable_nodes.end(); ++n) {
    foreach(xrefEditableNode * n, m_editable_nodes) {
        delete n;
    }

    foreach(xrefSourceNode *src_node, m_source_nodes) {
        // Make an editable node (which is also a scene item)
        auto editable = new xrefEditableNode(src_node->m_name);
        editable->m_src_node = src_node;
        editable->setRect(rand() % 1000, rand() % 500, 0, 0);
        editable->setBrush(QBrush(QColor(255, 255, 224)));
        editable->setFlag(QGraphicsItem::ItemIsMovable, true);
        editable->setFlag(QGraphicsItem::ItemIsSelectable, true);
        editable->setFlag(QGraphicsItem::ItemIsFocusable, true);
        m_editable_nodes[src_node->m_name] = editable;

        // add node as scene item to scene
        editable->setZValue(20);
        main->m_scene->addItem(editable);
    }

    foreach(xrefSourceNode * caller_src_node, m_source_nodes) {
        xrefEditableNode * caller_node = m_editable_nodes[caller_src_node->m_name];
        add_edges_to_scene(caller_node);
    }
}

// @private
// Directly use agsafeset which always works, contrarily to agset
int _agset(void * object, const QString & attr, const QString & value)
{
    return agsafeset(object, const_cast<char *>(qPrintable(attr)),
                     const_cast<char *>(qPrintable(value)),
                     const_cast<char *>(qPrintable(value)));
}

void xrefGraph::apply_layout(const char *gv_layout_method)
{
    QSet<xrefEditableNode *> enodes;
    foreach(auto n, m_editable_nodes.values()) { enodes.insert(n); }
    apply_layout(enodes, gv_layout_method);
}

void xrefGraph::apply_layout(const QSet<xrefEditableNode *> &nodes_affected, const char *gv_layout_method)
{
    //auto graph = new xrefGraphvizGraph();
    GVC_t * gvc = gvContext();
    Agraph_t * graph = agopen("G", AGDIGRAPH);

    QMap<xrefEditableNode *, Agnode_t *> enode_to_agnode;
    QMap<Agnode_t *, xrefEditableNode *> agnode_to_enode;

    // copy editable nodes to graphviz nodes
    foreach(xrefEditableNode * my_node, nodes_affected) {
        auto gv_node = agnode(graph, const_cast<char *>(qPrintable(my_node->m_name)));

        auto my_rect = my_node->rect();
        auto my_center = my_rect.center();

//        QString pos_str("%1,%2");
//        _agset(gv_node, "pos", pos_str.arg(my_center.x()).arg(my_center.y()));
        gv_node->u.coord.x = my_center.x();
        gv_node->u.coord.y = my_center.y();
        gv_node->u.bb.LL.x = my_rect.topLeft().x();
        gv_node->u.bb.LL.y = my_rect.topLeft().y();
        gv_node->u.bb.UR.x = my_rect.bottomRight().x();
        gv_node->u.bb.UR.y = my_rect.bottomRight().y();

        enode_to_agnode[my_node] = gv_node;
        agnode_to_enode[gv_node] = my_node;
    }

    // copy editable directed edges (they can overlap in opposite directions)
    foreach(xrefEditableNode * my_node, nodes_affected) {
        auto gv_from = enode_to_agnode[my_node];
        foreach(xrefEditableEdge * my_edge, my_node->m_linked_edges) {
            auto gv_to = enode_to_agnode[my_edge->m_dst];
            agedge(graph, gv_from, gv_to);
        }
    }

    // relayout
    gvLayout(gvc, graph, gv_layout_method);

    // copy coordinates back
    for (Agnode_t * agnode = agfstnode(graph); agnode != nullptr; agnode = agnxtnode(graph, agnode))
    {
//        QPointF pos((agnode->u.bb.LL.x + agnode->u.bb.UR.x) * 0.5,
//                    (agnode->u.bb.LL.y + agnode->u.bb.UR.y) * 0.5);
        QPointF pos(agnode->u.coord.x, agnode->u.coord.y);
        auto my_node = agnode_to_enode[agnode];

        auto rc = my_node->rect();
        QRectF new_rc(pos.x() - rc.width() * 0.5,
                      pos.y() - rc.height() * 0.5,
                      rc.width(), rc.height());

        // reposition center of rectangle on the point from graphviz
        my_node->set_rect_update_edges(new_rc);
    }

    // done
    agclose(graph);
    gvFreeContext(gvc);
}
