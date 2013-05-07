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
    unsigned long int m_next_node_id = 1;
    QMap<QString, unsigned long int> m_name_to_id;
    QMap<unsigned long int, QString> m_id_to_name;
    QMap<unsigned long int, xrefNode> m_nodes;

private:
    Ui::MainWindow *ui;

    void load_edges(const QString & fn);
    unsigned long int get_or_add_node_id(const QString & node_name);
};

#endif // MAIN_WINDOW_H
