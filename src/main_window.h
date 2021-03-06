#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

#include <QMainWindow>
#include <QMap>
#include <QSet>
#include <QString>
#include <QSettings>
#include <QGraphicsScene>
//#include <QGraphicsView>

#include "xref_node.h"
#include "xref_graph.h"
#include "xref_view.h"

namespace Ui {
class MainWindow;
}

//class xrefCanvasView;
//class xrefGraphWidget;
class xrefGraphvizGraph;
class xrefEditableNode;
class xrefSourceNode;

class QtVariantPropertyManager;
class QtTreePropertyBrowser;
class QtVariantProperty;
class QtProperty;
//class QtCanvas;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    static MainWindow * m_singleton;

    void selection_toggle(xrefEditableNode *node);
    void save_as_image(const QString & filename);

public:
    /// property finders (prop* by name, and name by prop*)
    QMap<QString, QtProperty *> m_name_to_property;
    QMap<QtProperty *, QString> m_property_to_name;

    /// Program options and settings
    QSettings m_settings;

    /// Stores all graph data which we are working on
    xrefGraph m_xrefgraph;

    /// Holds visible scene with all graph nodes, allows for very large object
    /// count and uses heavy 2D optimizations. Modeled after m_editable_nodes
    /// Scene cannot render itself, only contains stuff
    QGraphicsScene * m_scene = nullptr;

    /// Display component for scene. Renders scene contents
    xrefView * m_scene_view = nullptr;

private slots:
    void on_actionDot_triggered();
    void on_actionNeato_triggered();
    void on_actionFdp_triggered();
    void on_actionSfdp_triggered();
    void on_actionTwopi_triggered();
    void on_actionCirco_triggered();
    void on_actionPatchwork_triggered();
    void on_actionOsage_triggered();
    void on_actionSpline_triggered(bool checked);
    void on_property_value_changed(QtProperty *p, const QVariant &v);
    void on_actionReset_and_populate_triggered();
    void on_actionClear_everything_triggered();
    void on_actionSave_to_graph_png_triggered();
    void on_actionScalePlus20_triggered();
    void on_actionScaleMinus20_triggered();
    void on_actionModules_and_apps_triggered();

    void on_actionNew_view_triggered();

private:
    Ui::MainWindow * ui = nullptr;

    QtVariantPropertyManager * m_variant_manager;
    QtTreePropertyBrowser * m_property_editor;

private:
    void help_show_page(const QString & page);
    void construct_scene_view();

    /// Reads input JSON into source nodes, source nodes must remain read-only after
    /// they are loaded
    void load_source_nodes(const QString & fn);

    void add_property(QtVariantProperty *property, const QString &id);

    /// Generates list of editable nodes from source nodes (the first run). Erases
    /// all user work, so use this only once when editable nodes is empty
    void source_to_editable_nodes();

    /// Populates Views menu with names of available views
    void update_view_menu();
};

#endif // MAIN_WINDOW_H
