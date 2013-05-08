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

GraphRenderWidget::~GraphRenderWidget()
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

void GraphRenderWidget::paintEvent(QPaintEvent *)
{
    //graph_layout(false);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    auto main = MainWindow::m_singleton;
    QTransform tr;
    auto bb = main->m_graph->u.bb;
    tr = tr.translate(-bb.LL.x, bb.LL.y);
    double scalex = width() / (bb.UR.x - bb.LL.x);
    double scaley = height() / (bb.UR.y - bb.LL.y);
    tr = tr.scale(std::min(scalex, scaley), std::min(scalex, scaley));
    painter.setTransform(tr);

    for (auto node = agfstnode(main->m_graph); node != nullptr; node = agnxtnode(main->m_graph, node))
    {
        QRect rc(node->u.coord.x, node->u.coord.y, 20, 20);

        // draw edges
        for (Agedge_t * edge = agfstedge(main->m_graph, node);
             edge != nullptr; edge = agnxtedge(main->m_graph, edge, node)) {
            painter.setPen(Qt::darkGray);
            painter.drawLine(node->u.coord.x, node->u.coord.y,
                             edge->tail->u.coord.x, edge->tail->u.coord.y);
        }

        // draw box
        painter.fillRect(rc, Qt::white);

        painter.setPen(Qt::black);
        painter.drawRect(rc);

        painter.drawText(rc.topLeft() + QPoint(5, 15), node->name);
    }
}
