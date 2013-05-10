#include <QPainter>
#include <QApplication>
#include <QPalette>
#include <QGraphicsScene>

#include <graphviz/gvc.h>

#include "main_window.h" // do we want this dependency here?
#include "xref_node.h"
#include "xref_edge.h"

//xrefEditableNode::xrefEditableNode(): m_name()
//{
//}

xrefEditableNode::xrefEditableNode(const QString &name)
    : m_name(name)
{
}

QPointF xrefEditableNode::get_attach_point_for_edge()
{
    return sceneBoundingRect().center();
}

void xrefEditableNode::paint(QPainter *painter,
                             const QStyleOptionGraphicsItem *option,
                             QWidget *widget)
{
    auto bb = rect();
    QFontMetrics font_metrics = painter->fontMetrics();
    auto font_height = font_metrics.height();

    // update rect with text size
    if (bb.width() == 0) {
        bb.setWidth(font_metrics.width(m_name));
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
                      m_name);
}

void xrefEditableNode::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsRectItem::mouseMoveEvent(event);

    for(xrefEditableEdge * edge: m_linked_edges) {
        edge->update_edge_coords();
    }
}

QVariant xrefEditableNode::itemChange(QGraphicsItem::GraphicsItemChange change, const QVariant &value)
{
    if (change == QGraphicsItem::ItemSelectedChange)
    {
        // do we want this dependency here?
        auto main = MainWindow::m_singleton;

        if (value == true) {
            // do stuff if selected
            main->selection_toggle(this);
        }
        else {
            // do stuff if not selected
            main->selection_toggle(nullptr);
        }
    }

    return QGraphicsItem::itemChange(change, value);
}

