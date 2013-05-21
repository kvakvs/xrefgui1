#include <QPainter>
#include <QApplication>
#include <QPalette>
#include <QGraphicsScene>

#include <graphviz/gvc.h>

#include "main_window.h" // do we want this dependency here?
#include "xref_node.h"
#include "xref_edge.h"

xrefEditableNode::xrefEditableNode(const QString &name)
    : m_name(name)
{
}

xrefSceneNode::xrefSceneNode(xrefEditableNode * node)
    : QGraphicsRectItem(), m_node(node)
{
}

QPointF xrefSceneNode::get_attach_point_for_edge()
{
    return sceneBoundingRect().center();
}

void xrefSceneNode::paint(QPainter *painter,
                             const QStyleOptionGraphicsItem *option,
                             QWidget *widget)
{
    auto bb = rect();
    QFontMetrics font_metrics = painter->fontMetrics();
    auto font_height = font_metrics.height();

    // update rect with text size
    if (bb.width() == 0) {
        bb.setWidth(font_metrics.width(m_node->m_name));
        bb.setHeight(font_height);
        setRect(bb);
    }

    // box
    if (focusItem() == this) {
        auto color_selected = QApplication::palette().color(QPalette::Highlight);
        painter->fillRect(bb, QBrush(color_selected));
    } else {
        painter->fillRect(bb, brush());
    }
    painter->setPen(pen());
    painter->drawRect(bb);

    // text label
    painter->setPen(pen());
    auto text_start = bb.topLeft(); // where to draw name of the box
    painter->drawText(text_start.x(),
                      text_start.y() + font_height - font_metrics.descent(),
                      m_node->m_name);
}

void xrefSceneNode::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsRectItem::mouseMoveEvent(event);

    for(xrefSceneEdge * edge: m_linked_edges) {
        edge->update_scene_edge_coords();
    }
}

void xrefSceneNode::set_rect_update_edges(const QRectF &rect)
{
    setRect(rect);
    foreach(xrefSceneEdge * e, m_linked_edges) {
        e->update_scene_edge_coords();
    }
}

QVariant xrefSceneNode::itemChange(QGraphicsItem::GraphicsItemChange change, const QVariant &value)
{
    if (change == QGraphicsItem::ItemSelectedChange)
    {
        // do we want this dependency here?
//        auto main = MainWindow::m_singleton;
//        if (value == true) {
//            // do stuff if selected
//            main->selection_toggle(this);
//        }
//        else {
//            // do stuff if not selected
//            main->selection_toggle(nullptr);
//        }
    }

    return QGraphicsItem::itemChange(change, value);
}

