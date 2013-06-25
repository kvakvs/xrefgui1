#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QDebug>

#include "xref_code.h"
#include "xref_node.h"
#include "xref_edge.h"
#include "xref_graph.h"
#include "graphviz_graph.h"
#include "main_window.h"

xrefGraph::~xrefGraph()
{
    foreach(xrefSourceNode * n, m_source_nodes) { delete n; }
    m_source_nodes.clear();
    foreach(View * v, m_views) { delete v; }
    m_views.clear();
}

bool xrefGraph::view_new(const QString &name)
{
    if (m_views.contains(name)) { return false; }
    m_views.insert(name, new View(name));
    view_select(name);
    return true;
}

bool xrefGraph::view_select(const QString &name)
{
    if (!m_views.contains(name)) { return false; }
    m_view = m_views.value(name);
    return true;
}

bool xrefGraph::view_delete(const QString &name)
{
    if (!m_views.contains(name)) { return false; }
    delete m_views.value(name);
    m_views.remove(name);
    return true;
}

QList<QString> xrefGraph::view_get_names()
{
    QList<QString> result;
    foreach(View * v, m_views) {
        result.append(v->m_name);
    }
    return result;
}

const QString &xrefGraph::view_get_current()
{
    return m_view->m_name;
}

xrefGraph::View::~View() {
    clear();
}

void xrefGraph::View::clear()
{
    foreach(xrefEditableNode * n, m_editable_nodes) { delete n; }
    m_editable_nodes.clear();
}

void xrefGraph::add_edges_to_scene(xrefSceneNode_Module * caller_node)
{
    auto main = MainWindow::m_singleton;

    foreach(auto callee, caller_node->m_node->m_src_node->m_callees)
    {
        // if scene contains no callee node
        if (! m_scene_nodes.contains(callee)) { continue; }

        xrefSceneNode_Module * callee_node = m_scene_nodes.value(callee);
        Q_ASSERT(callee_node != nullptr);

        // if edge exists
        if (caller_node->has_edge(caller_node, callee_node)) { continue; }

        auto edge = new xrefSceneEdge(caller_node, callee_node);
        edge->setZValue(10);
        caller_node->m_linked_edges.append(edge);
        callee_node->m_linked_edges.append(edge);
        main->m_scene->addItem(edge);
    }
}

/// Load module caller/callee dependencies list
void xrefGraph::load_mod_calls(const QJsonObject & jroot)
{
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

}

/// Load application list and modules belonging to apps
void xrefGraph::load_apps(const QJsonObject & jroot)
{
    // input -> {applications: {app1: ['mod1', 'mod2']}}
    auto applications = jroot.value("applications").toObject();
    for (auto aiter = applications.begin(); aiter != applications.end(); ++aiter)
    {
        auto app_name = aiter.key();

        auto c_app = new xrefCApp();
        c_app->m_name = app_name;
        m_code_apps.insert(app_name, c_app);

        auto modules = aiter.value().toArray();
        for (auto m_iter = modules.begin(); m_iter != modules.end(); ++m_iter)
        {
            auto mod_name = (* m_iter).toString();

            auto c_mod = new xrefCMod();
            c_mod->m_name = mod_name;
            c_mod->m_app = c_app;
            m_code_modules.insert(mod_name, c_mod);

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
}

/// Load function call graph edge list
void xrefGraph::load_fun_calls(const QJsonObject & jroot)
{
    // input -> {connections: {mod1: ['mod2', 'mod3']}}
    auto calls = jroot.value("calls").toObject();
    for (auto n1iter = calls.begin(); n1iter != calls.end(); ++n1iter)
    {
        auto caller_name = n1iter.key();
        auto caller_fun = code_get_or_create_fun(caller_name);
        caller_fun->m_name = caller_name;

        auto value_list = n1iter.value().toArray();
        for (auto n2iter = value_list.begin(); n2iter != value_list.end(); ++n2iter)
        {
            auto callee_name = (* n2iter).toString();
            auto callee_fun = code_get_or_create_fun(callee_name);
            caller_fun->m_callees.append(callee_fun);
        } // for callee list
    } // for json keys

}

xrefCFun *xrefGraph::code_get_or_create_fun(const QString &full_name)
{
    if (m_code_functions.contains(full_name)) { return m_code_functions.value(full_name); }
    auto f = new xrefCFun();
    f->m_full_name = full_name;

    // Given full function name parse it to app name, module name and function name
    QString mod_name, app_name, fun_name;
    code_parse_fun_name(full_name, app_name, mod_name, fun_name);

    f->m_name = fun_name;
    f->m_app = code_get_app(app_name);
    f->m_module = code_get_mod(mod_name);

    m_code_functions.insert(full_name, f);
    return f;
}

xrefCFun *xrefGraph::code_get_fun(const QString &name)
{
    if (m_code_functions.contains(name)) { return m_code_functions.value(name); }
    return nullptr;
}

xrefCMod *xrefGraph::code_get_mod(const QString &name)
{
    if (m_code_modules.contains(name)) { return m_code_modules.value(name); }
    return nullptr;
}

xrefCApp *xrefGraph::code_get_app(const QString &name)
{
    if (m_code_apps.contains(name)) { return m_code_apps.value(name); }
    return nullptr;
}

bool xrefGraph::code_parse_fun_name(const QString &full_name, QString & out_app,
                                    QString & out_mod, QString & out_fun)
{
    // Parse rules for Erlang. Given function name in form of "module:function/arity"
    // extracts module and application name
    QStringList m_fa = full_name.split(':');
    QStringList f_a = m_fa.last().split('/');

    out_mod = m_fa.length() > 0 ? m_fa.first() : QString();

    xrefCApp * app = code_get_app_by_module_name(m_fa.first());
    out_app = app ? app->m_name : QString();

    out_fun = f_a.length() > 0 ? f_a.first() : QString();
    return true;
}

xrefCApp *xrefGraph::code_get_app_by_module_name(const QString &mod_name)
{
    xrefCMod * mod = code_get_mod(mod_name);
    if (!mod) return nullptr;
    return mod->m_app;
}

void xrefGraph::load_source_nodes(const QString &fn)
{
    QFile file(fn);
    if (file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        auto bytes = file.readAll();
        auto jdoc = QJsonDocument::fromJson(bytes);
        auto jroot = jdoc.object();

        load_mod_calls(jroot);
        load_apps(jroot);
        load_fun_calls(jroot);
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
        m_view->m_editable_nodes.insert(src_node->m_name, editable);
    }
}

void xrefGraph::recreate_scene_from_editable(const QSet<QString> & node_names)
{
    auto main = MainWindow::m_singleton;
    main->m_scene->clear();
    m_scene_nodes.clear();

    // create scene nodes
    foreach(xrefEditableNode *ed_node, m_view->m_editable_nodes) {
        Q_ASSERT(ed_node != nullptr);

        // if user not interested in this node
        if (! node_names.contains(ed_node->m_name)) { continue; }

        // if node already exists in scene - skip
        //if (m_scene_nodes.contains(ed_node->m_name)) { continue; }

        // Make an editable node (which is also a scene item)
        auto scene_node = new xrefSceneNode_Module(ed_node);

        auto & pos = ed_node->m_position;
        auto & sz = ed_node->m_scene_size;
//        scene_node->setPos(pos);
        scene_node->setRect(pos.x() - sz.width()/2, pos.y() - sz.height()/2,
                            sz.width(), sz.height());
        scene_node->setFlag(QGraphicsItem::ItemIsMovable, true);
        scene_node->setFlag(QGraphicsItem::ItemIsSelectable, true);
        scene_node->setFlag(QGraphicsItem::ItemIsFocusable, true);
        scene_node->setBrush(choose_brush(ed_node, scene_node));

        m_scene_nodes.insert(ed_node->m_name, scene_node);

        // add node as scene item to scene
        scene_node->setZValue(20);
        main->m_scene->addItem(scene_node);
    }

    // create edges
    foreach(xrefSourceNode * caller_src_node, m_source_nodes) {
        auto caller_src_name = caller_src_node->m_name;

        // if user not interested in this node
        if (! node_names.contains(caller_src_name)) { continue; }
        // if scene does not contains caller
        //if (! m_scene_nodes.contains(caller_src_name)) { continue; }

        xrefSceneNode_Module * caller_node = m_scene_nodes.value(caller_src_name);
        Q_ASSERT(caller_node != nullptr);

        add_edges_to_scene(caller_node);
    }
    main->m_scene->setSceneRect(main->m_scene->itemsBoundingRect());
    main->m_scene->update();
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
    QSet<xrefSceneNode_Module *> s_nodes;
    foreach(auto n, m_scene_nodes.values()) {
        if (n) s_nodes.insert(n);
    }
    apply_layout(s_nodes, gv_layout_method);
}

void xrefGraph::apply_layout(const QSet<xrefSceneNode_Module *> &nodes_affected, const char *gv_layout_method)
{
    //auto graph = new xrefGraphvizGraph();
    GVC_t * gvc = gvContext();
    Agraph_t * graph = agopen(const_cast<char *>("G"), AGDIGRAPH);

    QMap<xrefSceneNode_Module *, Agnode_t *> snode_to_agnode;
    QMap<Agnode_t *, xrefSceneNode_Module *> agnode_to_snode;
    QMap<QString, Agraph_t *> subgraphs;

    // copy editable nodes to graphviz nodes
    foreach(xrefSceneNode_Module * my_node, nodes_affected) {
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
    foreach(xrefSceneNode_Module * my_node, nodes_affected) {
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
        xrefSceneNode_Module * my_node = agnode_to_snode.value(agnode);

        auto rc = my_node->rect();
        QRectF new_rc(pos.x() - rc.width() * 0.5,
                      pos.y() - rc.height() * 0.5,
                      rc.width(), rc.height());

        // reposition center of rectangle on the point from graphviz
        my_node->set_rect_update_edges(new_rc);
    }
    save_scene_to_editable();

    // done
    foreach(Agraph_t * g, subgraphs) { agclose(g); }
    agclose(graph);
    gvFreeContext(gvc);
}

void xrefGraph::clear_editable()
{
    m_view->clear();
    //m_app_modules.clear();
    m_scene_nodes.clear();
    m_selected_nodes.clear();
}

void xrefGraph::transform(const QTransform &tr)
{
    foreach(xrefEditableNode *ed_node, m_view->m_editable_nodes) {
        Q_ASSERT(ed_node != nullptr);
        if (m_scene_nodes.contains(ed_node->m_name)) {
            xrefSceneNode_Module * scene_node = m_scene_nodes.value(ed_node->m_name);
            auto rc = scene_node->rect();

            auto diff = tr.map(rc.center()) - rc.center();

            scene_node->set_rect_update_edges(rc.translated(diff));
        }
    }
}

void xrefGraph::save_scene_to_editable()
{
    foreach(xrefSceneNode_Module * s_node, m_scene_nodes) {
        auto ed_node = s_node->m_node;
        ed_node->m_position = s_node->rect().center();
        ed_node->m_scene_size = s_node->rect().size();
    }
}

QBrush xrefGraph::choose_brush(xrefEditableNode *ed_node, xrefSceneNode_Module *scene_node)
{
    quint16 sum = qChecksum((const char *)ed_node->m_app_name.data(),
                            ed_node->m_app_name.length());
    QColor c((sum & 0x1F) * 127 / 0x1F + 128,
             ((sum >> 5) & 0x3F) * 127 / 0x3F + 128,
             ((sum >> 11) & 0x1F) * 127 / 0x1F + 128);
    return QBrush(c);
}
