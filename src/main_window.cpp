//#include <QGraphicsScene>
//#include <QGraphicsView>
#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

#include "main_window.h"
#include "ui_main_window.h"
#include "graph_render_widget.h"
#include "graph.h"

MainWindow * MainWindow::m_singleton = nullptr;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    m_singleton = this;

    ui->setupUi(this);
    load_edges("edges.json");

    m_graph_widget = new GraphRenderWidget(this);
    ui->centralWidget->layout()->addWidget(m_graph_widget);
    m_graph_widget->graph_layout(true);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::load_edges(const QString &fn)
{
    aginit();
    m_graph = agopen("xrefgraph", AGDIGRAPH/*STRICT*/);

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
            auto node1 = get_or_add_node(node1_name);

            auto value_list = n1iter.value().toArray();
            for (auto n2iter = value_list.begin(); n2iter != value_list.end(); ++n2iter)
            {
                auto node2_name = (* n2iter).toString();
                auto node2 = get_or_add_node(node2_name);

                auto edge = agedge(m_graph, node1, node2);
            }
        }
    }
}

Agnode_t * MainWindow::get_or_add_node(const QString &node_name)
{
    auto iter = m_name_to_agnode.find(node_name);
    if (iter == m_name_to_agnode.end()) {
        char node_name_c[128];
        strncpy(node_name_c, node_name.toLocal8Bit(), sizeof(node_name_c));
        node_name_c[sizeof(node_name_c)-1] = 0;

        auto newnode = agnode(m_graph, node_name_c);
        m_name_to_agnode[node_name] = newnode;
    }
    return iter.value();
}

void MainWindow::on_actionDot_triggered()
{
    m_graph_widget->graph_layout(0);
    this->update();
}
