#include <QPainter>

#include <math.h>
#include "main_window.h"
#include "graph_render_widget.h"

#include <boost/graph/random_layout.hpp>
#include <boost/graph/topology.hpp>
#include <boost/random/linear_congruential.hpp> // for random
#include <boost/graph/fruchterman_reingold.hpp> // for force layout & cooling
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
//pos_vector_t position_vec(num_vertices(main->m_graph));
pos_vector_t position_vec;

typedef std::vector<xref_point_type> pos_vector_t;
typedef boost::iterator_property_map<pos_vector_t::iterator,
        boost::property_map<xrefGraph, boost::vertex_index_t>::type>
        pos_map_t;
//pos_map_t position(position_vec.begin(),
//                   boost::get(boost::vertex_index, main->m_graph));
pos_map_t position;

void GraphRenderWidget::graph_layout(bool init_flag)
{
    auto main = MainWindow::m_singleton;

    boost::minstd_rand rnd_gen;

    xref_topology_type topo(rnd_gen, 0, 0, 1250, 700);

    if (init_flag) {
        position_vec.resize(num_vertices(main->m_graph));
        position = pos_map_t(position_vec.begin(),
                             boost::get(boost::vertex_index, main->m_graph));
        boost::random_graph_layout(main->m_graph, position, topo);
    } else {
        const int ITERATIONS = 5;
        boost::fruchterman_reingold_force_directed_layout
                (main->m_graph, position, topo, cooling(progress_cooling(ITERATIONS)));
    }

    //------
    boost::graph_traits<xrefGraph>::vertex_iterator vi, vi_end;
    for (boost::tie(vi, vi_end) = boost::vertices(main->m_graph); vi != vi_end; ++vi) {
        const auto x = position[*vi][0];
        const auto y = position[*vi][1];
        //const auto name = boost::get(boost::vertex_name, main->m_graph, *vi);
        main->m_nodes[*vi].m_rect.setRect(x, y, 20, 20);
    }
}

void GraphRenderWidget::paintEvent(QPaintEvent *)
{
    graph_layout(false);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    auto main = MainWindow::m_singleton;
//    for (auto iter = main->m_nodes.begin(); iter != main->m_nodes.end(); ++iter) {
//        xrefNode & n = *iter;

    // http://www.boost.org/doc/libs/1_38_0/libs/graph/doc/quick_tour.html
    // BGL_FORALL_VERTICES_T(v, main->m_graph, xrefGraph)
    typedef boost::property_map<xrefGraph, boost::vertex_index_t>::type index_map_t;
    index_map_t index = boost::get(boost::vertex_index, main->m_graph);
    for (auto vp = boost::vertices(main->m_graph); vp.first != vp.second; ++vp.first)
    {
        auto vertex_id = index[* vp.first];
        auto & n = main->m_nodes[vertex_id];
        painter.setPen(Qt::black);
        painter.drawRect(n.m_rect);
        painter.drawText(n.m_rect.topLeft() + QPoint(5, 15), n.m_name);

        /*for (auto & out_edge_name: n.m_edges_out) {
            auto & out_node = main->m_nodes[out_edge_name];
            painter.setPen(Qt::darkGray);
            painter.drawLine(n.m_rect.bottomRight(), out_node.m_rect.bottomRight());
        }*/
    }
//    painter.setPen(Qt::darkGreen);
//    painter.drawRect(1*50, 2*50, 6*50, 4*50);
//    painter.setPen(Qt::darkGray);
//    painter.drawLine(2*50, 8*50, 6*50, 2*50);
}
