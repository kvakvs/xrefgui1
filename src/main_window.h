#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

#include <QMainWindow>
#include <QMap>
#include <QSet>
#include <QString>
#include <QSettings>
#include <QGraphicsScene>
#include <QGraphicsView>

#include "xref_node.h"

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

public:
    /// property finders (prop* by name, and name by prop*)
    QMap<QString, QtProperty *> m_name_to_property;
    QMap<QtProperty *, QString> m_property_to_name;

    /// Temporary holder for graphviz graph (for relayout)
    xrefGraphvizGraph * m_gv = nullptr;

    /// JSON data decoded is stored here read-only, this is used to fill
    /// m_editable_nodes without sizes, positions and other attributes
    QMap<QString, xrefSourceNode *> m_source_nodes;

    /// Editable items saved separately, modelled after m_source_nodes. User is
    /// allowed to do changes to this structure. This is used to fill m_scene
    QMap<QString, xrefEditableNode *> m_editable_nodes;

    /// Selection on screen
    QSet<xrefEditableNode *> m_selected_nodes;

    /// Program options and settings
    QSettings m_settings;

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

private:
    Ui::MainWindow * ui = nullptr;

    /// Holds visible scene with all graph nodes, allows for very large object
    /// count and uses heavy 2D optimizations. Modeled after m_editable_nodes
    /// Scene cannot render itself, only contains stuff
    QGraphicsScene * m_scene = nullptr;

    /// Display component for scene. Renders scene contents
    QGraphicsView * m_scene_view = nullptr;

    QtVariantPropertyManager * m_variant_manager;
    QtTreePropertyBrowser * m_property_editor;

private:
    /// Reads input JSON into source nodes, source nodes must remain read-only after
    /// they are loaded
    void load_source_nodes(const QString & fn);

    void add_property(QtVariantProperty *property, const QString &id);

    /// Generates list of editable nodes from source nodes (the first run). Erases
    /// all user work, so use this only once when editable nodes is empty
    void source_to_editable_nodes();
};

#endif // MAIN_WINDOW_H
