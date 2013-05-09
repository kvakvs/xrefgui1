#ifndef GRAPH_RENDER_WIDGET_H
#define GRAPH_RENDER_WIDGET_H

//#include <QGraphicsScene>
#include <QWidget>


class xrefGraphWidget : public QWidget
{
    Q_OBJECT
public:
    explicit xrefGraphWidget(QWidget *parent = 0);
    virtual ~xrefGraphWidget();

protected:
    void paintEvent(QPaintEvent *);
    //void mousePressEvent(QMouseEvent *);

    //QGraphicsScene scene;

signals:

public slots:
};

#endif // GRAPH_RENDER_WIDGET_H
