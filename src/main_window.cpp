//#include <QGraphicsScene>
//#include <QGraphicsView>
#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include "qtvariantproperty.h"
#include "qttreepropertybrowser.h"
//#include "qtcanvas.h"

#include "main_window.h"
#include "ui_main_window.h"
#include "graph_render_widget.h"
#include "xref_node.h"
//#include "canvas_view.h"
#include "graphviz_graph.h"

MainWindow * MainWindow::m_singleton = nullptr;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent), ui(new Ui::MainWindow),
    m_settings()
{
    m_singleton = this;

    ui->setupUi(this);

//    m_canvas = new QtCanvas(800, 600);
//    m_canvas_view = new xrefCanvasView(m_canvas, this);
//    setCentralWidget(m_canvas_view);

    m_scene = new QGraphicsScene();
    m_scene_view = new QGraphicsView(m_scene, this);
    setCentralWidget(m_scene_view);

    load_source_nodes("edges.json");
    source_to_editable_nodes();

    //m_graph_widget->graph_layout(true);

    // property manager
    m_variant_manager = new QtVariantPropertyManager(this);

    connect(m_variant_manager, SIGNAL(valueChanged(QtProperty *, const QVariant &)),
                this, SLOT(valueChanged(QtProperty *, const QVariant &)));

    QtVariantEditorFactory *variantFactory = new QtVariantEditorFactory(this);

    //canvas = new QtCanvas(800, 600);
    //canvasView = new CanvasView(canvas, this);
    //setCentralWidget(canvasView);

    //QDockWidget *dock = new QDockWidget(this);
    //addDockWidget(Qt::RightDockWidgetArea, dock);

    m_property_editor = new QtTreePropertyBrowser(ui->dockWidget0);
    m_property_editor->setFactoryForManager(m_variant_manager, variantFactory);
    ui->dockWidget0->setWidget(m_property_editor);

    connect(m_variant_manager, SIGNAL(valueChanged(QtProperty *, const QVariant &)),
            this, SLOT(on_property_value_changed(QtProperty *, const QVariant &)));
}

MainWindow::~MainWindow()
{
    foreach(auto n, m_source_nodes) { delete n; }
    m_source_nodes.clear();

    foreach(auto n, m_editable_nodes) { delete n; }
    m_editable_nodes.clear();

    delete ui;
}

void MainWindow::selection_toggle(xrefEditableNode * node)
{
//    if (m_selected_nodes.contains(node)) {
//        m_selected_nodes.remove(node);
//    } else {
//        m_selected_nodes.insert(node);
//    }
    m_selected_nodes.clear();
    m_selected_nodes.insert(node);

    // property editor
    QtVariantProperty * property;
    m_property_editor->clear();
    m_variant_manager->clear();
    m_name_to_property.clear();
    m_property_to_name.clear();

    property = m_variant_manager->addProperty(QVariant::Bool, tr("Draw IN edges"));
    property->setValue(QVariant(node->m_draw_in_edges));
    add_property(property, QLatin1String("draw_in_edges"));
    //m_property_editor->addProperty(property);

    property = m_variant_manager->addProperty(QVariant::Bool, tr("Draw OUT edges"));
    property->setValue(QVariant(node->m_draw_out_edges));
    add_property(property, QLatin1String("draw_out_edges"));
    //m_property_editor->addProperty(property);
}


void MainWindow::add_property(QtVariantProperty *property, const QString &id)
{
    m_property_to_name[property] = id;
    m_name_to_property[id] = property;
    /*QtBrowserItem * item =*/ m_property_editor->addProperty(property);
    //if (idToExpanded.contains(id))
    //    propertyEditor->setExpanded(item, idToExpanded[id]);
}

void MainWindow::source_to_editable_nodes()
{
    // CLEAR USER EDITS! do not call this more than once after first import
    foreach(auto n, m_editable_nodes) { delete n; }
    m_editable_nodes.clear();

    foreach(xrefSourceNode * s, m_source_nodes) {
        auto e = new xrefEditableNode(s->m_name);
        e->setRect(rand() % 1000, rand() % 500, 0, 0);
        m_editable_nodes.append(e);

        m_scene->addItem(e);
    }
}

void MainWindow::on_property_value_changed(QtProperty *p, const QVariant &v)
{
    auto prop_name = m_property_to_name[p];

    foreach(xrefEditableNode * sel, m_selected_nodes) {
        if (prop_name == "draw_in_edges") {
            sel->m_draw_in_edges = v.toBool();
        }
        if (prop_name == "draw_out_edges") {
            sel->m_draw_out_edges = v.toBool();
        }
    }
    m_scene_view->update();
}

void MainWindow::load_source_nodes(const QString &fn)
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

            m_source_nodes.append(src_node);
        } // for json keys
    } // if file
}

//Agnode_t * MainWindow::get_or_add_node(const QString &node_name)
//{
//    auto iter = m_name_to_agnode.find(node_name);
//    if (iter == m_name_to_agnode.end()) {
//        char node_name_c[128];
//        strncpy(node_name_c, node_name.toLocal8Bit(), sizeof(node_name_c));
//        node_name_c[sizeof(node_name_c)-1] = 0;

//        Agnode_t * graphviz_node = agnode(m_graph, node_name_c);
////        _agset(newnode, "fixedsize", "true");
////        _agset(newnode, "height", "90");
////        _agset(newnode, "width", "15");
//        if (node_name == "ejabberd" || node_name == "ejabberd_router") {
//            m_selected_nodes.insert(graphviz_node);
//        }
//        m_node_info[node_name] = xrefEditableNode(node_name, graphviz_node);
//        m_name_to_agnode[node_name] = graphviz_node;
//    }
//    return m_name_to_agnode[node_name];
//}

void MainWindow::on_actionDot_triggered() {
    if (m_gv) m_gv->redo_layout("dot");
}

void MainWindow::on_actionNeato_triggered() {
    if (m_gv) m_gv->redo_layout("neato");
}

void MainWindow::on_actionFdp_triggered() {
    if (m_gv) m_gv->redo_layout("fdp");
}

void MainWindow::on_actionSfdp_triggered() {
    if (m_gv) m_gv->redo_layout("sfdp");
}

void MainWindow::on_actionTwopi_triggered() {
    if (m_gv) m_gv->redo_layout("twopi");
}

void MainWindow::on_actionCirco_triggered() {
    if (m_gv) m_gv->redo_layout("circo");
}

void MainWindow::on_actionPatchwork_triggered() {
    if (m_gv) m_gv->redo_layout("patchwork");
}

void MainWindow::on_actionOsage_triggered() {
    if (m_gv) m_gv->redo_layout("osage");
}

void MainWindow::on_actionSpline_triggered(bool checked)
{
    m_settings.setValue("layout/spline", checked);
    if (m_gv) m_gv->redo_layout(nullptr);
}
