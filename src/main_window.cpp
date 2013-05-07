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

#include <boost/graph/labeled_graph.hpp>

MainWindow * MainWindow::m_singleton = nullptr;

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    m_singleton = this;

    ui->setupUi(this);
    load_edges("edges.json");

    QWidget * w = new GraphRenderWidget(this);
    ui->centralWidget->layout()->addWidget(w);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::load_edges(const QString &fn)
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
            auto node1_id = get_or_add_node_id(node1_name);
//            if (! boost::vertex_by_label(node1_name, m_graph)) {
//                boost::add_vertex(node1_name, m_graph);
//            }
//            auto & node1 = get_or_add_node(node1_name);

            auto value_list = n1iter.value().toArray();
            for (auto n2iter = value_list.begin(); n2iter != value_list.end(); ++n2iter)
            {
                auto node2_name = (* n2iter).toString();
                auto node2_id = get_or_add_node_id(node2_name);
//                if (! boost::vertex_by_label(node2_name, m_graph)) {
//                    boost::add_vertex(node2_name, m_graph);
//                }
//                boost::add_edge_by_label(node1_name, node2_name, m_graph);
                boost::add_edge(node1_id, node2_id, m_graph);

//                auto & node2 = get_or_add_node(node2_name);
//                node1.m_edges_out.insert(node2_name);
//                node2.m_edges_in.insert(node1_name);
            }
        }
    }
}

unsigned long MainWindow::get_or_add_node_id(const QString &node_name)
{
    auto iter = m_name_to_id.find(node_name);
    if (iter == m_name_to_id.end()) {
        xrefNode n(node_name);
        n.m_rect.setRect(0, 0, 90, 50);

        m_nodes[m_next_node_id] = n;
        m_name_to_id[node_name] = m_next_node_id;
        m_id_to_name[m_next_node_id] = node_name;
        return m_next_node_id++;
    }
    return *iter;
}

//xrefNode &MainWindow::get_or_add_node(const QString &node_name)
//{
//    auto iter = m_existing_node_names.find(node_name);
//    if (iter == m_existing_node_names.end()) {
//        xrefNode n(node_name);
//        m_nodes[node_name] = n;
//    }
//    return m_nodes[node_name];
//}

