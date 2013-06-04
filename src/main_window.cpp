#include <QTransform>
#include <QMessageBox>

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
#include "select_nodes_dialog.h"

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
    m_xrefgraph.load_source_nodes("input.json");
    m_xrefgraph.view_new(tr("default"));
    update_view_menu();

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

    showMaximized();
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
        property->setValue(QVariant(node->m_editor_flags.edges_in));
        add_property(property, QLatin1String("draw_in_edges"));

        property = m_variant_manager->addProperty(QVariant::Bool, tr("Draw OUT edges"));
        property->setValue(QVariant(node->m_editor_flags.edges_out));
        add_property(property, QLatin1String("draw_out_edges"));
    }
}

void MainWindow::save_as_image(const QString &filename)
{
    // Selections would also render to the file
    m_scene->clearSelection();
    // Re-shrink the scene to it's bounding contents
    m_scene->setSceneRect(m_scene->itemsBoundingRect());

    // Create the image with the exact size of the shrunk scene
    auto sz = m_scene->sceneRect().size().toSize();
    if (sz.width() >= 32768 || sz.height() >= 32768) {
        QMessageBox msgBox;
        auto F = tr("Working area for saving PNG is too large (%1 x %2) limit is 32768 in either dimension");
        msgBox.setText(QString(F).arg(sz.width()).arg(sz.height()));
        msgBox.exec();
        return;
    }

    QImage image(sz, QImage::Format_ARGB32);
    // Start all pixels transparent
    image.fill(Qt::white);

    QPainter painter(&image);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setRenderHint(QPainter::TextAntialiasing);
    m_scene->render(&painter);
    image.save(filename);
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
            sel->m_editor_flags.edges_in = v.toBool();
        }
        if (prop_name == "draw_out_edges") {
            sel->m_editor_flags.edges_out = v.toBool();
        }
    }
    m_scene_view->update();
}


void MainWindow::on_actionDot_triggered() {
    m_xrefgraph.apply_layout("dot");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionNeato_triggered() {
    m_xrefgraph.apply_layout("neato");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionFdp_triggered() {
    m_xrefgraph.apply_layout("fdp");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionSfdp_triggered() {
    m_xrefgraph.apply_layout("sfdp");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionTwopi_triggered() {
    m_xrefgraph.apply_layout("twopi");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionCirco_triggered() {
    m_xrefgraph.apply_layout("circo");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionPatchwork_triggered() {
    m_xrefgraph.apply_layout("patchwork");
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene_view->update();
}

void MainWindow::on_actionOsage_triggered() {
//    m_xg.redo_layout("osage");
}

void MainWindow::on_actionSpline_triggered(bool checked)
{
//    m_settings.setValue("layout/spline", checked);
//    m_xg.redo_layout(nullptr);
}

void MainWindow::on_actionReset_and_populate_triggered()
{
    auto dialog = new SelectNodesDialog(
                this, m_xrefgraph.m_app_modules.keys(), // app names
                m_xrefgraph.m_source_nodes.values(), // module names
                QList<QString>() // selected
                );
    if (dialog->exec() == QDialog::Accepted) {
        m_scene->clear();
        m_xrefgraph.clear_editable();
        m_xrefgraph.source_to_editable_nodes();
        m_xrefgraph.recreate_scene_from_editable(dialog->m_selected_modules);
    }
}

void MainWindow::on_actionModules_and_apps_triggered()
{
    auto dialog = new SelectNodesDialog(
                this, m_xrefgraph.m_app_modules.keys(), // app names
                m_xrefgraph.m_source_nodes.values(), // module names
                m_xrefgraph.m_scene_nodes.keys() // selected
                );
    if (dialog->exec() == QDialog::Accepted) {
        m_xrefgraph.save_scene_to_editable();
        m_xrefgraph.recreate_scene_from_editable(dialog->m_selected_modules);
    }
}

void MainWindow::on_actionClear_everything_triggered()
{
    m_scene->clear();
    m_xrefgraph.clear_editable();
    m_scene->update();
}

void MainWindow::on_actionSave_to_graph_png_triggered()
{
    save_as_image("graph.png");
}

void MainWindow::on_actionScalePlus20_triggered()
{
    auto bb = m_scene->itemsBoundingRect();
    auto center = bb.center();
    QTransform tr;
    tr.translate(-center.x(), -center.y());
    tr.scale(1.2, 1.2);
    tr.translate(center.x(), center.y());
    m_xrefgraph.transform(tr);
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene->update();
}

void MainWindow::on_actionScaleMinus20_triggered()
{
    auto bb = m_scene->itemsBoundingRect();
    auto center = bb.center();
    QTransform tr;
    tr.translate(-center.x(), -center.y());
    tr.scale(1.0/1.2, 1.0/1.2);
    tr.translate(center.x(), center.y());
    m_xrefgraph.transform(tr);
    m_scene->setSceneRect(m_scene->itemsBoundingRect());
    m_scene->update();
}


void MainWindow::update_view_menu()
{
    auto a1 = ui->actionSwitch_view;
    auto m1 = new QMenu(this);
    foreach(auto ename, m_xrefgraph.view_get_names()) {
        auto act = m1->addAction(ename);
        if (m_xrefgraph.view_get_current() == ename) {
            act->setEnabled(false);
            act->setCheckable(true);
            act->setChecked(true);
        }
    }
    a1->setMenu(m1);

    auto a2 = ui->actionDelete_view;
    auto m2 = new QMenu(this);
    foreach(auto ename, m_xrefgraph.view_get_names()) {
        auto act = m2->addAction(ename);
        if (m_xrefgraph.view_get_current() == ename) {
            act->setEnabled(false);
            act->setCheckable(true);
            act->setChecked(true);
        }
    }
    a2->setMenu(m2);
}

void MainWindow::on_actionNew_view_triggered()
{
    //m_xrefgraph.view_new()
}
