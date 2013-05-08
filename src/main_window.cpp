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
    QMainWindow(parent), ui(new Ui::MainWindow),
    m_settings()
{
    m_singleton = this;

    ui->setupUi(this);

    m_graph_widget = new GraphRenderWidget(this);
    ui->centralWidget->layout()->addWidget(m_graph_widget);

    m_gvc = gvContext();
    load_edges("edges.json");
    //m_graph_widget->graph_layout(true);
}

MainWindow::~MainWindow()
{
    gvFreeContext(m_gvc);
    delete ui;
}

void MainWindow::selection_toggle(const QString &, Agnode_t * node)
{
    if (m_selected_nodes.contains(node)) {
        m_selected_nodes.remove(node);
    } else {
        m_selected_nodes.insert(node);
    }
}

// Directly use agsafeset which always works, contrarily to agset
static inline int _agset(void * object, const QString & attr, const QString & value)
{
    return agsafeset(object, const_cast<char *>(qPrintable(attr)),
                     const_cast<char *>(qPrintable(value)),
                     const_cast<char *>(qPrintable(value)));
}

void MainWindow::load_edges(const QString &fn)
{
    m_graph = agopen("xrefgraph", AGDIGRAPH/*STRICT*/);
    //_agset(m_graph, "area", "0,0,1300,700");
    _agset(m_graph, "splines", "false");

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

                if (node1 != node2) {
                    auto edge = agedge(m_graph, node1, node2);
                }
            }
        }
    }
    gvLayout(m_gvc, m_graph, "dot");
}

Agnode_t * MainWindow::get_or_add_node(const QString &node_name)
{
    auto iter = m_name_to_agnode.find(node_name);
    if (iter == m_name_to_agnode.end()) {
        char node_name_c[128];
        strncpy(node_name_c, node_name.toLocal8Bit(), sizeof(node_name_c));
        node_name_c[sizeof(node_name_c)-1] = 0;

        Agnode_t * graphviz_node = agnode(m_graph, node_name_c);
//        _agset(newnode, "fixedsize", "true");
//        _agset(newnode, "height", "90");
//        _agset(newnode, "width", "15");
        if (node_name == "ejabberd" || node_name == "ejabberd_router") {
            m_selected_nodes.insert(graphviz_node);
        }
        m_node_info[node_name] = xrefNode(node_name, graphviz_node);
        m_name_to_agnode[node_name] = graphviz_node;
    }
    return m_name_to_agnode[node_name];
}

void MainWindow::on_actionDot_triggered() {
    redo_layout("dot");
}

void MainWindow::on_actionNeato_triggered() {
    redo_layout("neato");
}

void MainWindow::on_actionFdp_triggered() {
    redo_layout("fdp");
}

void MainWindow::on_actionSfdp_triggered() {
    redo_layout("sfdp");
}

void MainWindow::on_actionTwopi_triggered() {
    redo_layout("twopi");
}

void MainWindow::on_actionCirco_triggered() {
    redo_layout("circo");
}

void MainWindow::on_actionPatchwork_triggered() {
    redo_layout("patchwork");
}

void MainWindow::on_actionOsage_triggered() {
    redo_layout("osage");
}

void MainWindow::redo_layout(const char * algo) {
    bool use_spline = m_settings.value("layout/spline", false).toBool();

    if (use_spline) { _agset(m_graph, "splines", "true"); }
    else { _agset(m_graph, "splines", "false"); }

    gvFreeLayout(m_gvc, m_graph);
    gvLayout(m_gvc, m_graph, algo);
    this->update();
}

void MainWindow::on_actionSpline_triggered(bool checked)
{
    m_settings.setValue("layout/spline", checked);
    redo_layout("dot");
}

void MainWindow::on_actionDraw_in_triggered(bool checked)
{
    m_settings.setValue("render/draw_in_edges", checked);
    this->update();
}

void MainWindow::on_actionDraw_out_triggered(bool checked)
{
    m_settings.setValue("render/draw_out_edges", checked);
    this->update();
}
