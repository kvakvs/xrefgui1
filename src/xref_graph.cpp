#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QDebug>

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

void xrefGraph::add_edges_to_scene(xrefSceneNode * caller_node)
{
    auto main = MainWindow::m_singleton;

    foreach(auto callee, caller_node->m_node->m_src_node->m_callees) {
        if (! m_scene_nodes.contains(callee)) continue;
        xrefSceneNode * callee_node = m_scene_nodes.value(callee);

        auto edge = new xrefSceneEdge(caller_node, callee_node);
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

        //-------------------------------------------------
        // Load module caller/callee dependencies list
        //-------------------------------------------------
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

            m_source_nodes.insert(node1_name, src_node);
        } // for json keys

        //-------------------------------------------------
        // Load application list and modules belonging to apps
        //-------------------------------------------------
        // input -> {applications: {app1: ['mod1', 'mod2']}}
        auto applications = jroot.value("applications").toObject();
        for (auto aiter = applications.begin(); aiter != applications.end(); ++aiter)
        {
            auto app_name = aiter.key();
            auto modules = aiter.value().toArray();
            for (auto m_iter = modules.begin(); m_iter != modules.end(); ++m_iter)
            {
                auto mod_name = (* m_iter).toString();
                if (! m_app_modules.contains(app_name)) {
                    m_app_modules.insert(app_name, QList<QString>());
                }
                m_app_modules[app_name].append(mod_name);

                // also add app name to module node
                if (m_source_nodes.contains(mod_name)) {
                    auto node = m_source_nodes.value(mod_name);
                    node->m_app_name = app_name;
                }
            } // for module list
        }
    } // if file
}

void xrefGraph::source_to_editable_nodes()
{
    // CLEAR USER EDITS! do not call this more than once after first import
    clear_editable();

    foreach(xrefSourceNode *src_node, m_source_nodes) {
        Q_ASSERT(src_node != nullptr);
        // Make an editable node (which is also a scene item)
        auto editable = new xrefEditableNode(src_node->m_name);
        editable->m_app_name = src_node->m_app_name;
        editable->m_src_node = src_node;
        m_editable_nodes.insert(src_node->m_name, editable);
    }
}

void xrefGraph::editable_to_scene_nodes(const QSet<QString> & node_names)
{
    auto main = MainWindow::m_singleton;

    main->m_scene->clear();

    foreach(xrefEditableNode *ed_node, m_editable_nodes) {
        Q_ASSERT(ed_node != nullptr);
        if (! node_names.contains(ed_node->m_name)) { continue; }

        // Make an editable node (which is also a scene item)
        auto scene_node = new xrefSceneNode(ed_node);
        scene_node->setRect(rand() % 1000, rand() % 500, 0, 0);
        scene_node->setBrush(QBrush(QColor(255, 255, 224)));
        scene_node->setFlag(QGraphicsItem::ItemIsMovable, true);
        scene_node->setFlag(QGraphicsItem::ItemIsSelectable, true);
        scene_node->setFlag(QGraphicsItem::ItemIsFocusable, true);
        scene_node->setBrush(choose_brush(ed_node, scene_node));
        m_scene_nodes.insert(ed_node->m_name, scene_node);

        // add node as scene item to scene
        scene_node->setZValue(20);
        main->m_scene->addItem(scene_node);
    }

    foreach(xrefSourceNode * caller_src_node, m_source_nodes) {
        auto caller_src_name = caller_src_node->m_name;
        if (! node_names.contains(caller_src_name)) { continue; }
        if (! m_scene_nodes.contains(caller_src_name)) { continue; }
        xrefSceneNode * caller_node = m_scene_nodes.value(caller_src_name);

        Q_ASSERT(caller_node != nullptr);
        add_edges_to_scene(caller_node);
    }
    main->m_scene->setSceneRect(main->m_scene->itemsBoundingRect());
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
    QSet<xrefSceneNode *> s_nodes;
    foreach(auto n, m_scene_nodes.values()) {
        if (n) s_nodes.insert(n);
    }
    apply_layout(s_nodes, gv_layout_method);
}

void xrefGraph::apply_layout(const QSet<xrefSceneNode *> &nodes_affected, const char *gv_layout_method)
{
    //auto graph = new xrefGraphvizGraph();
    GVC_t * gvc = gvContext();
    Agraph_t * graph = agopen(const_cast<char *>("G"), AGDIGRAPH);

    QMap<xrefSceneNode *, Agnode_t *> snode_to_agnode;
    QMap<Agnode_t *, xrefSceneNode *> agnode_to_snode;
    QMap<QString, Agraph_t *> subgraphs;

    // copy editable nodes to graphviz nodes
    foreach(xrefSceneNode * my_node, nodes_affected) {
        // get or make subgraph
        if (! subgraphs.contains(my_node->m_app_name)) {
            subgraphs.insert(my_node->m_app_name,
                             agsubg(graph, const_cast<char *>(qPrintable(my_node->m_name)))
                             );
        }
        Agraph_t * subg = subgraphs.value(my_node->m_app_name);

        // note: node name here must be unique
        auto gv_node = agnode(
                    subg,
                    const_cast<char *>(qPrintable(my_node->m_name))
                    );

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

        snode_to_agnode.insert(my_node, gv_node);
        agnode_to_snode.insert(gv_node, my_node);
    }

    // copy editable directed edges (they can overlap in opposite directions)
    foreach(xrefSceneNode * my_node, nodes_affected) {
        Agnode_t * gv_from = snode_to_agnode.value(my_node);

        foreach(xrefSceneEdge * my_edge, my_node->m_linked_edges) {
            Agnode_t * gv_to = snode_to_agnode.value(my_edge->m_dst);
            agedge(graph, gv_from, gv_to);
        }
    }

    // relayout
    gvLayout(gvc, graph, gv_layout_method);

    // copy coordinates back
    for (Agnode_t * agnode = agfstnode(graph); agnode != nullptr; agnode = agnxtnode(graph, agnode))
    {
        QPointF pos(agnode->u.coord.x, agnode->u.coord.y);
        xrefSceneNode * my_node = agnode_to_snode.value(agnode);

        auto rc = my_node->rect();
        QRectF new_rc(pos.x() - rc.width() * 0.5,
                      pos.y() - rc.height() * 0.5,
                      rc.width(), rc.height());

        // reposition center of rectangle on the point from graphviz
        my_node->set_rect_update_edges(new_rc);
    }

    // done
    foreach(Agraph_t * g, subgraphs) { agclose(g); }
    agclose(graph);
    gvFreeContext(gvc);
}

void xrefGraph::clear_editable()
{
    foreach(xrefEditableNode * n, m_editable_nodes) {
        delete n;
    }
    m_editable_nodes.clear();
    //m_app_modules.clear();
    m_scene_nodes.clear();
    m_selected_nodes.clear();
}

void xrefGraph::transform(const QTransform &tr)
{
    foreach(xrefEditableNode *ed_node, m_editable_nodes) {
        Q_ASSERT(ed_node != nullptr);
        if (m_scene_nodes.contains(ed_node->m_name)) {
            xrefSceneNode * scene_node = m_scene_nodes.value(ed_node->m_name);
            auto rc = scene_node->rect();

            auto diff = tr.map(rc.center()) - rc.center();

            scene_node->set_rect_update_edges(rc.translated(diff));
        }
    }
}

QBrush xrefGraph::choose_brush(xrefEditableNode *ed_node, xrefSceneNode *scene_node)
{
    quint16 sum = qChecksum((const char *)ed_node->m_app_name.data(),
                            ed_node->m_app_name.length());
    QColor c((sum & 0x1F) * 127 / 0x1F + 128,
             ((sum >> 5) & 0x3F) * 127 / 0x3F + 128,
             ((sum >> 11) & 0x1F) * 127 / 0x1F + 128);
    return QBrush(c);
}
