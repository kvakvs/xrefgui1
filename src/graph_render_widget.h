#ifndef GRAPH_RENDER_WIDGET_H
#define GRAPH_RENDER_WIDGET_H

//#include <QGraphicsScene>
#include <QWidget>
#include <graphviz/gvc.h>
//#include <graphviz/cgraph.h>

class GraphRenderWidget : public QWidget
{
    Q_OBJECT
public:
    explicit GraphRenderWidget(QWidget *parent = 0);
    virtual ~GraphRenderWidget();
    void graph_layout(int type);

protected:
    void paintEvent(QPaintEvent *);

    //QGraphicsScene scene;
    GVC_t * m_gvc;

signals:

public slots:
};

#endif // GRAPH_RENDER_WIDGET_H
