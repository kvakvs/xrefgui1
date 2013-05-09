#include <QPainter>
#include <QApplication>
#include <QMouseEvent>

#include <math.h>

#include "main_window.h"
#include "graph_render_widget.h"
#include "render_things.h"
#include "xref_node.h"


xrefGraphWidget::xrefGraphWidget(QWidget *parent) :
    QWidget(parent)
{
//    QObject::connect(this, QWidget::mousePressEvent,
//    [this](QMouseEvent * e) { this->on_clicked(e); })
}

xrefGraphWidget::~xrefGraphWidget()
{
}

void xrefGraphWidget::paintEvent(QPaintEvent *)
{
    //graph_layout(false);

    QPainter painter(this);

    QFontMetrics font_metrics = painter.fontMetrics();
    painter.setRenderHint(QPainter::Antialiasing);
    auto main = MainWindow::m_singleton;

#if 0
    // settings
//    bool draw_out_edges = main->m_settings.value("render/draw_out_edges", true).toBool();
//    bool draw_in_edges = main->m_settings.value("render/draw_in_edges", false).toBool();
    bool use_spline = main->m_settings.value("layout/spline", false).toBool();

    auto color_selected = QApplication::palette().color(QPalette::Highlight);

    // size and scale
    auto bb = main->m_graph->u.bb;
    double translatex = -bb.LL.x;
    double translatey = -bb.LL.y;
    double scalex = width() / (bb.UR.x - bb.LL.x);
    double scaley = height() / (bb.UR.y - bb.LL.y);

    for (Agnode_t * node = agfstnode(main->m_graph);
         node != nullptr; node = agnxtnode(main->m_graph, node))
    {
        QPointF n1pos((node->u.coord.x + translatex) * scalex,
                      (node->u.coord.y + translatey) * scaley);

        // text rectangle, also used as box for node
        auto font_height = font_metrics.height();
        QRectF node_bbox(n1pos.x(), n1pos.y(),
                         font_metrics.width(node->name), font_height);

        auto & node1_info = main->m_node_info[node->name];

        bool is_node_selected = main->m_selected_nodes.contains(node);

        // draw OUT edges
        if (node1_info.m_draw_out_edges) {
            painter.setPen(Qt::red);
            for (Agedge_t * edge = agfstedge(main->m_graph, node);
                 edge != nullptr; edge = agnxtedge(main->m_graph, edge, node))
            {
                if (node != edge->head) continue;

                if (use_spline) {
                    DrawThings::spline(painter, edge,
                                       translatex, translatey, scalex, scaley);
                } else {
                    QPointF n2pos((edge->tail->u.coord.x + translatex) * scalex,
                                  (edge->tail->u.coord.y + translatey) * scaley);
                    DrawThings::arrow(painter, n1pos, n2pos);
                }
            }
        }

        // draw IN edges
        painter.setPen(Qt::blue);
        for (Agedge_t * edge = agfstin(main->m_graph, node);
             edge != nullptr; edge = agnxtin(main->m_graph, edge))
        {
            auto & node2_info = main->m_node_info[edge->tail->name];
            if (! node2_info.m_draw_in_edges) continue;
            //                if (! main->m_selected_nodes.contains(edge->tail)) continue;

            if (use_spline) {
                DrawThings::spline(painter, edge,
                                   translatex, translatey, scalex, scaley);
            } else {
                QPointF n2pos((edge->tail->u.coord.x + translatex) * scalex,
                              (edge->tail->u.coord.y + translatey) * scaley);
                DrawThings::arrow(painter, n1pos, n2pos);
            }
        }

        // draw box
        QPointF half_box_size(node_bbox.width() / 2.0, node_bbox.height() / 2.0);
        node_bbox.translate(-half_box_size);
        n1pos -= half_box_size;

        // draw bounding box and rectangle around it
        painter.fillRect(node_bbox, Qt::white);
        painter.setPen(Qt::black);
        painter.drawRect(node_bbox);
        // save calculated box to xrefNode struct
        node1_info.m_rect = node_bbox;

        // text label
        painter.setPen(Qt::black);
        painter.drawText(n1pos.x(), n1pos.y() + font_height - font_metrics.descent(), node->name);

        // if selected, draw selection mark
        if (is_node_selected) {
            QRectF selected_bbox(node_bbox);
            selected_bbox.adjust(-3, -3, +3, +3);
            painter.setPen(QPen(color_selected, 2));
            painter.drawRect(selected_bbox);
        }
    }
#endif
}
/*
void xrefGraphWidget::mousePressEvent(QMouseEvent * e)
{
    auto main = MainWindow::m_singleton;

    foreach(xrefEditableNode & nodeinfo, main->m_node_info) {
        if (nodeinfo.m_rect.contains(e->x(), e->y())) {
            main->selection_toggle(nodeinfo.m_name, nodeinfo.m_graphviz_node);
        }
    }
    main->update();
}
*/
