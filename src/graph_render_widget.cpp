#include <QPainter>

#include <math.h>
#include "main_window.h"
#include "graph_render_widget.h"

#include <boost/graph/random_layout.hpp>

GraphRenderWidget::GraphRenderWidget(QWidget *parent) :
    QWidget(parent)
{
}

void GraphRenderWidget::layout()
{
//    auto main = MainWindow::m_singleton;
//    boost::random_graph_layout(main->m_graph, )

//    int i = 0;
//    int total = main->m_nodes.size();

//    auto half_width = main->width() / 2.0f;
//    auto half_height = main->height() / 2.0f;

//    for (auto iter = main->m_nodes.begin(); iter != main->m_nodes.end(); ++iter) {
//        xrefNode & n = *iter;

//        float x = half_width + half_width * 0.8f * cos(6.28f * i / total);
//        float y = half_height + half_height * 0.8f * sin(6.28f * i / total);

//        n.m_rect.setRect(x, y, 90, 40);
//        i++;
//    }
}

void GraphRenderWidget::paintEvent(QPaintEvent *)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    layout();

    auto main = MainWindow::m_singleton;
//    for (auto iter = main->m_nodes.begin(); iter != main->m_nodes.end(); ++iter) {
//        xrefNode & n = *iter;

    // http://www.boost.org/doc/libs/1_38_0/libs/graph/doc/quick_tour.html
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
