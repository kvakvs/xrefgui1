#ifndef XREF_VIEW_H
#define XREF_VIEW_H

#include <QGraphicsView>

class QGraphicsScene;

/// Custom QGraphicsView descendant with added pan/view/selection events
class xrefView : public QGraphicsView
{
    Q_OBJECT
public:
    explicit xrefView(QWidget * parent = 0);
    explicit xrefView(QGraphicsScene * scene, QWidget * parent = 0);

signals:

public slots:
    void wheelEvent(QWheelEvent *event);

private:
    void xref_connect_slots();
};

#endif // XREF_VIEW_H
