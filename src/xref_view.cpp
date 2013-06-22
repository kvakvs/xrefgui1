#include <QWheelEvent>
#include <QApplication>

#include "xref_view.h"

xrefView::xrefView(QWidget *parent) :
    QGraphicsView(parent)
{
    xref_connect_slots();
}

xrefView::xrefView(QGraphicsScene * s, QWidget *parent) :
    QGraphicsView(s, parent)
{
    xref_connect_slots();
}

void xrefView::wheelEvent(QWheelEvent *event)
{
    static const double ZOOMQ = 1.03;
    if (QApplication::keyboardModifiers() & Qt::ControlModifier) {
        auto d = event->delta();
        if (d < 0) {
            QGraphicsView::scale(1.0 / ZOOMQ, 1.0 / ZOOMQ);
        } else if (d > 0) {
            QGraphicsView::scale(ZOOMQ, ZOOMQ);
        }
    }
}

void xrefView::xref_connect_slots()
{
    //QObject::connect(this, QGraphicsView::wheelEvent, xrefView::wheelEvent);
}
