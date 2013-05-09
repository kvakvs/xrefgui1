#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

#include <QMainWindow>
#include <QMap>
#include <QSet>
#include <QString>
#include <QSettings>

#include "xref_node.h"

namespace Ui {
class MainWindow;
}
class GraphRenderWidget;
class QtVariantPropertyManager;
class QtTreePropertyBrowser;
class QtVariantProperty;
class QtProperty;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    static MainWindow * m_singleton;

    void selection_toggle(const QString &name, Agnode_t *node);

    // graph definition
    Agraph_t * m_graph;
    GVC_t * m_gvc;

    //unsigned long int m_next_node_id = 1;
    QMap<QString, Agnode_t *> m_name_to_agnode;
    // currently selected item or items in editor
    QSet<Agnode_t *> m_selected_nodes;
    // additional attributes like rectangle on screen, pin, type etc
    QMap<QString, xrefNode> m_node_info;
    QMap<QString, QtProperty *> m_name_to_property;
    QMap<QtProperty *, QString> m_property_to_name;

    QSettings m_settings;
    QtVariantPropertyManager * m_variant_manager;
    QtTreePropertyBrowser * m_property_editor;

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
    GraphRenderWidget * m_graph_widget = nullptr;

    void load_edges(const QString & fn);
    Agnode_t * get_or_add_node(const QString & node_name);
    void redo_layout(const char * algo);
    void add_property(QtVariantProperty *property, const QString &id);
};

#endif // MAIN_WINDOW_H
