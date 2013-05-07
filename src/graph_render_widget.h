#ifndef GRAPH_RENDER_WIDGET_H
#define GRAPH_RENDER_WIDGET_H

//#include <QGraphicsScene>
#include <QWidget>

class GraphRenderWidget : public QWidget
{
    Q_OBJECT
public:
    explicit GraphRenderWidget(QWidget *parent = 0);

protected:
    void paintEvent(QPaintEvent *);
    void layout();

    //QGraphicsScene scene;

signals:
    
public slots:
};

#endif // GRAPH_RENDER_WIDGET_H
