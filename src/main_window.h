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

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    static MainWindow * m_singleton;
    // graph definition
    xrefGraph m_graph;
//    QSet<QString> m_existing_node_names;
//    QMap<QString, xrefNode> m_nodes;

private:
    Ui::MainWindow *ui;

    void load_edges(const QString & fn);
    xrefNode & get_or_add_node(const QString & node_name);
};

#endif // MAIN_WINDOW_H
