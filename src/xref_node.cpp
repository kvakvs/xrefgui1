#include <QPainter>
#include <QApplication>
#include <QPalette>

#include <graphviz/gvc.h>

#include "xref_node.h"

xrefEditableNode::xrefEditableNode(): m_name()
{
}

xrefEditableNode::xrefEditableNode(const QString &name)
    : m_name(name)
{
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
