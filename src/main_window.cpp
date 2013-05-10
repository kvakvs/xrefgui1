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
//#include "canvas_view.h"
#include "graphviz_graph.h"
#include "xref_node.h"
#include "xref_edge.h"

MainWindow * MainWindow::m_singleton = nullptr;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent), ui(new Ui::MainWindow),
    m_settings()
{
    m_singleton = this;
    ui->setupUi(this);

    // scene and sceneview
    m_scene = new QGraphicsScene();
    m_scene_view = new QGraphicsView(m_scene, this);
    m_scene_view->setRenderHint(QPainter::Antialiasing, false);
    setCentralWidget(m_scene_view);

    // load JSON data and populate view
    load_source_nodes("edges.json");
    source_to_editable_nodes();

    // property manager and property editor tab
    m_variant_manager = new QtVariantPropertyManager(this);

    connect(m_variant_manager, SIGNAL(valueChanged(QtProperty *, const QVariant &)),
                this, SLOT(valueChanged(QtProperty *, const QVariant &)));

    QtVariantEditorFactory *variantFactory = new QtVariantEditorFactory(this);

    m_property_editor = new QtTreePropertyBrowser(ui->dockWidget0);
    m_property_editor->setFactoryForManager(m_variant_manager, variantFactory);
    ui->dockWidget0->setWidget(m_property_editor);

    connect(m_variant_manager, SIGNAL(valueChanged(QtProperty *, const QVariant &)),
            this, SLOT(on_property_value_changed(QtProperty *, const QVariant &)));
}

MainWindow::~MainWindow()
{
//    for(auto n = m_source_nodes.begin(); n != m_source_nodes.end(); ++n) {
//        delete n.value();
//    }
    foreach(xrefSourceNode * n, m_source_nodes) {
        delete n;
    }
    m_source_nodes.clear();

//    for(auto n = m_editable_nodes.begin(); n != m_editable_nodes.end(); ++n) {
//        delete n.value();
//    }
    foreach(xrefEditableNode * n, m_editable_nodes) {
        delete n;
    }
    m_editable_nodes.clear();

    delete ui;
}

void MainWindow::selection_toggle(xrefEditableNode * node)
{
    m_selected_nodes.clear();
    if (node) {
        m_selected_nodes.insert(node);
    }

    // property editor
    m_property_editor->clear();
    m_variant_manager->clear();
    m_name_to_property.clear();
    m_property_to_name.clear();

    if (node) {
        QtVariantProperty * property;

        property = m_variant_manager->addProperty(QVariant::Bool, tr("Draw IN edges"));
        property->setValue(QVariant(node->m_draw_in_edges));
        add_property(property, QLatin1String("draw_in_edges"));

        property = m_variant_manager->addProperty(QVariant::Bool, tr("Draw OUT edges"));
        property->setValue(QVariant(node->m_draw_out_edges));
        add_property(property, QLatin1String("draw_out_edges"));
    }
}


void MainWindow::add_property(QtVariantProperty *property, const QString &id)
{
    m_property_to_name[property] = id;
    m_name_to_property[id] = property;
    /*QtBrowserItem * item =*/ m_property_editor->addProperty(property);
}

void MainWindow::source_to_editable_nodes()
{
    // CLEAR USER EDITS! do not call this more than once after first import
    //for(auto n = m_editable_nodes.begin(); n != m_editable_nodes.end(); ++n) {
    foreach(xrefEditableNode * n, m_editable_nodes) {
        delete n;
    }

    foreach(xrefSourceNode * s, m_source_nodes) {
        // Make an editable node (which is also a scene item)
        auto editable = new xrefEditableNode(s->m_name);
        editable->setRect(rand() % 1000, rand() % 500, 0, 0);
        editable->setBrush(QBrush(QColor(255, 255, 224)));
        editable->setFlag(QGraphicsItem::ItemIsMovable, true);
        editable->setFlag(QGraphicsItem::ItemIsSelectable, true);
        editable->setFlag(QGraphicsItem::ItemIsFocusable, true);
        m_editable_nodes[s->m_name] = editable;

        // add node as scene item to scene
        editable->setZValue(20);
        m_scene->addItem(editable);
    }

    foreach(xrefSourceNode * caller_src_node, m_source_nodes) {
        xrefEditableNode * caller_node = m_editable_nodes[caller_src_node->m_name];
        //auto p1 = node1->rect().topLeft();

        for(auto callee: caller_src_node->m_callees) {
            xrefEditableNode * callee_node = m_editable_nodes[callee];
            //auto p2 = callee_node->rect().topLeft();

            auto edge = new xrefEditableEdge(caller_node, callee_node);
            edge->setZValue(10);
            caller_node->m_linked_edges.append(edge);
            callee_node->m_linked_edges.append(edge);
            m_scene->addItem(edge);
        }
    }
    m_scene_view->update();
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

            m_source_nodes[node1_name] = src_node;
        } // for json keys
    } // if file
}

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
