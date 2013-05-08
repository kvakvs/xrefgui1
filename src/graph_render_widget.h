#ifndef GRAPH_RENDER_WIDGET_H
#define GRAPH_RENDER_WIDGET_H

//#include <QGraphicsScene>
#include <QWidget>

class GraphRenderWidget : public QWidget
{
    Q_OBJECT
public:
    explicit GraphRenderWidget(QWidget *parent = 0);
    void graph_layout(int type);

protected:
    void paintEvent(QPaintEvent *);

    //QGraphicsScene scene;

signals:

public slots:
};

#endif // GRAPH_RENDER_WIDGET_H
