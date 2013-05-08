#include <QPainter>

#include <math.h>
#include "main_window.h"
#include "graph_render_widget.h"

#include <boost/graph/random_layout.hpp>
#include <boost/graph/topology.hpp>
#include <boost/random/linear_congruential.hpp> // for random
#include <boost/graph/fruchterman_reingold.hpp> // for force layout & cooling
#include <boost/graph/kamada_kawai_spring_layout.hpp>
//#include <boost/progress.hpp>

GraphRenderWidget::GraphRenderWidget(QWidget *parent) :
    QWidget(parent)
{
    m_gvc = gvContext();
}

GraphRenderWidget::~GraphRenderWidget()
{
    gvFreeContext(m_gvc);
}

class progress_cooling : public boost::linear_cooling<double>
{
    typedef boost::linear_cooling<double> inherited;

public:
    explicit progress_cooling(std::size_t iterations) : inherited(iterations) {
        //display.reset(new boost::progress_display(iterations + 1, std::cerr));
    }
    double operator()() {
        //++(*display);
        return inherited::operator()();
    }
private:
    //boost::shared_ptr<boost::progress_display> display;
};

// type 0: dot
// type 1: 20 steps of fruchterman-reingold
// type 2: kamada-kawai
void GraphRenderWidget::graph_layout(int type)
{
    auto main = MainWindow::m_singleton;

    switch (type) {
    case 0: {
        gvLayout(m_gvc, main->m_graph, "dot");
        break;
    }
    case 1: {
//        const int ITERATIONS = 20;
//        boost::fruchterman_reingold_force_directed_layout
//                (main->m_graph, g_position, topo, cooling(progress_cooling(ITERATIONS)));
//        break;
    }
    case 2: {
//        auto weight = boost::get(boost::edge_weight, main->m_graph);
//        boost::kamada_kawai_spring_layout(main->m_graph, g_position, weight,
//                                          boost::side_length<double>(50.0));
    }
    }
}

void GraphRenderWidget::paintEvent(QPaintEvent *)
{
    //graph_layout(false);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    auto main = MainWindow::m_singleton;
    QTransform tr;
    painter.setTransform(tr.translate(width()/2.0, height()/2.0));

    // draw edges
//    for (auto edge = agfstedge(main->m_graph); node != nullptr; node = agnxtedge()) {
//        //auto edge_index = boost::get(boost::edge_index, main->m_graph, edge);
//        auto & node1 = main->m_nodes[boost::source(edge, main->m_graph)];
//        auto & node2 = main->m_nodes[boost::target(edge, main->m_graph)];
//        painter.setPen(Qt::darkGray);
//        painter.drawLine(node1.m_rect.bottomRight(), node2.m_rect.bottomRight());
//    }

    for (auto node = agfstnode(main->m_graph); node != nullptr; node = agnxtnode(main->m_graph, node))
    {
        QRect rc(node->u.coord.x, node->u.coord.y, 20, 20);

        painter.fillRect(rc, Qt::white);

        painter.setPen(Qt::black);
        painter.drawRect(rc);

        painter.drawText(rc.topLeft() + QPoint(5, 15), node->name);
    }
}
