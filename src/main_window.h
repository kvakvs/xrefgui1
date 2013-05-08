#ifndef MAIN_WINDOW_H
#define MAIN_WINDOW_H

#include <QMainWindow>
#include <QMap>
#include <QSet>
#include <QString>

#include "graph.h"

namespace Ui {
class MainWindow;
}
class GraphRenderWidget;

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    static MainWindow * m_singleton;

    // graph definition
    Agraph_t * m_graph;
    GVC_t * m_gvc;

    unsigned long int m_next_node_id = 1;
    QMap<QString, Agnode_t *> m_name_to_agnode;
    //QMap<QString, xrefNode> m_nodes;

private slots:
    void on_actionDot_triggered();

    void on_actionNeato_triggered();

    void on_actionFdp_triggered();

    void on_actionSfdp_triggered();

    void on_actionTwopi_triggered();

    void on_actionCirco_triggered();

    void on_actionPatchwork_triggered();

    void on_actionOsage_triggered();

private:
    Ui::MainWindow * ui = nullptr;
    GraphRenderWidget * m_graph_widget = nullptr;

    void load_edges(const QString & fn);
    Agnode_t * get_or_add_node(const QString & node_name);
};

#endif // MAIN_WINDOW_H
