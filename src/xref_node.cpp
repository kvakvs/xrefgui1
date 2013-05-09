#include <QPainter>

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
    painter->fillRect(bb, Qt::white);
    painter->setPen(Qt::black);
    painter->drawRect(bb);

    // text label
    painter->setPen(Qt::black);
    auto text_start = bb.topLeft(); // where to draw name of the box
    painter->drawText(text_start.x(),
                      text_start.y() + font_height - font_metrics.descent(),
                      m_name);
}
