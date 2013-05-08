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
    QFontMetrics font_metrics = painter.fontMetrics();
    painter.setRenderHint(QPainter::Antialiasing);

    auto main = MainWindow::m_singleton;
    auto bb = main->m_graph->u.bb;
    double translatex = -bb.LL.x;
    double translatey = -bb.LL.y;
    double scalex = width() / (bb.UR.x - bb.LL.x);
    double scaley = height() / (bb.UR.y - bb.LL.y);

    for (auto node = agfstnode(main->m_graph); node != nullptr; node = agnxtnode(main->m_graph, node))
    {
        QPoint n1pos((node->u.coord.x + translatex) * scalex,
                     (node->u.coord.y + translatey) * scaley);

        // draw edges
        for (Agedge_t * edge = agfstedge(main->m_graph, node);
             edge != nullptr; edge = agnxtedge(main->m_graph, edge, node))
        {
            QPoint n2pos((edge->tail->u.coord.x + translatex) * scalex,
                         (edge->tail->u.coord.y + translatey) * scaley);
            painter.setPen(Qt::darkGray);
            painter.drawLine(n1pos, n2pos);
        }

        // draw box
        auto font_height = font_metrics.height();
        QRect rect(n1pos.x(), n1pos.y(), font_metrics.width(node->name), font_height);
        painter.fillRect(rect, Qt::white);
        painter.setPen(Qt::black);
        painter.drawRect(rect);

        painter.drawText(n1pos.x(), n1pos.y() + font_height - font_metrics.descent(), node->name);
    }
}
