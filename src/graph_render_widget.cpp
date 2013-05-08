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

typedef std::vector<xref_point_type> pos_vector_t;
pos_vector_t g_position_vec;

typedef std::vector<xref_point_type> pos_vector_t;
typedef boost::iterator_property_map<pos_vector_t::iterator,
        boost::property_map<xrefGraph, boost::vertex_index_t>::type>
        pos_map_t;
pos_map_t g_position;

// type 0: random
// type 1: 20 steps of fruchterman-reingold
// type 2: kamada-kawai
void GraphRenderWidget::graph_layout(int type)
{
    auto main = MainWindow::m_singleton;
    boost::minstd_rand rnd_gen;
    xref_topology_type topo(rnd_gen, -width()/2, -height()/2, width()/2, height()/2);

    switch (type) {
    case 0: {
        g_position_vec.resize(num_vertices(main->m_graph));
        g_position = pos_map_t(g_position_vec.begin(),
                               boost::get(boost::vertex_index, main->m_graph));
        boost::random_graph_layout(main->m_graph, g_position, topo);
//        boost::random_graph_layout(main->m_graph, boost::get(vertex_position, main->m_graph), topo);
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

    //------
    boost::graph_traits<xrefGraph>::vertex_iterator vi, vi_end;
//    auto position = boost::get(vertex_position, main->m_graph);
    for (boost::tie(vi, vi_end) = boost::vertices(main->m_graph); vi != vi_end; ++vi) {
        const auto x = g_position[*vi][0];
        const auto y = g_position[*vi][1];
        main->m_nodes[*vi].m_rect.setRect(x-10, y-10, 20, 20);
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
    BGL_FORALL_EDGES_T(edge, main->m_graph, xrefGraph) {
        //auto edge_index = boost::get(boost::edge_index, main->m_graph, edge);
        auto & node1 = main->m_nodes[boost::source(edge, main->m_graph)];
        auto & node2 = main->m_nodes[boost::target(edge, main->m_graph)];
        painter.setPen(Qt::darkGray);
        painter.drawLine(node1.m_rect.bottomRight(), node2.m_rect.bottomRight());
    }

    // http://www.boost.org/doc/libs/1_38_0/libs/graph/doc/quick_tour.html
    // BGL_FORALL_VERTICES_T(v, main->m_graph, xrefGraph)
    typedef boost::property_map<xrefGraph, boost::vertex_index_t>::type index_map_t;
    index_map_t index = boost::get(boost::vertex_index, main->m_graph);
    for (auto vp = boost::vertices(main->m_graph); vp.first != vp.second; ++vp.first)
    {
        auto vertex_id = index[* vp.first];
        auto & n = main->m_nodes[vertex_id];

        painter.fillRect(n.m_rect, Qt::white);

        painter.setPen(Qt::black);
        painter.drawRect(n.m_rect);

        painter.drawText(n.m_rect.topLeft() + QPoint(5, 15), n.m_name);
    }
}
