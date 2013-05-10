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
