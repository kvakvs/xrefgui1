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
    m_xrefgraph.load_source_nodes("edges.json");
    m_xrefgraph.source_to_editable_nodes();

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
    delete ui;
}

void MainWindow::selection_toggle(xrefEditableNode * node)
{
    m_xrefgraph.m_selected_nodes.clear();
    if (node) {
        m_xrefgraph.m_selected_nodes.insert(node);
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

void MainWindow::on_property_value_changed(QtProperty *p, const QVariant &v)
{
    auto prop_name = m_property_to_name[p];

    foreach(xrefEditableNode * sel, m_xrefgraph.m_selected_nodes) {
        if (prop_name == "draw_in_edges") {
            sel->m_draw_in_edges = v.toBool();
        }
        if (prop_name == "draw_out_edges") {
            sel->m_draw_out_edges = v.toBool();
        }
    }
    m_scene_view->update();
}


void MainWindow::on_actionDot_triggered() {
    m_xrefgraph.apply_layout("dot");
    m_scene_view->update();
}

void MainWindow::on_actionNeato_triggered() {
    m_xrefgraph.apply_layout("neato");
    m_scene_view->update();
}

void MainWindow::on_actionFdp_triggered() {
//    m_xg.redo_layout("fdp");
}

void MainWindow::on_actionSfdp_triggered() {
//    m_xg.redo_layout("sfdp");
}

void MainWindow::on_actionTwopi_triggered() {
    m_xrefgraph.apply_layout("twopi");
    m_scene_view->update();
}

void MainWindow::on_actionCirco_triggered() {
//    m_xg.redo_layout("circo");
}

void MainWindow::on_actionPatchwork_triggered() {
//    m_xg.redo_layout("patchwork");
}

void MainWindow::on_actionOsage_triggered() {
//    m_xg.redo_layout("osage");
}

void MainWindow::on_actionSpline_triggered(bool checked)
{
//    m_settings.setValue("layout/spline", checked);
//    m_xg.redo_layout(nullptr);
}
