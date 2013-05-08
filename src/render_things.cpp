#include <QLine>
#include <QPolygon>

#include <cmath>
#include <graphviz/gvc.h>

#include "render_things.h"

static const double ARROW_SIZE = 15.0;
static const double ARROW_SWING = 3.0; // how wide is arrow in pixels (half swing)

void DrawThings::line_with_offset_from_p1(QPainter &p, const QPointF &p1,
                                          const QPointF &p2, double offset)
{
    QLineF line(p1, p2);

    QLineF dir_vec(line);
    dir_vec.setLength(offset);
    QPointF begin_offset(dir_vec.dx(), dir_vec.dy());

    p.drawLine(line.p1() + begin_offset, line.p2());
}

void DrawThings::arrow(QPainter &p, const QPointF &p1, const QPointF &p2)
{
    QLineF line(p1, p2);

    p.setPen(Qt::darkGray);
    p.drawLine(line);
    //line_with_offset_from_p1(p, p1, p2, 20);

    arrow_head(p, line, p2, ARROW_SIZE, ARROW_SWING);
}

void DrawThings::arrow_head(QPainter &p, const QLineF & line, const QPointF &point,
                            double size, double swing)
{
    // line direction vector
    QLineF dir_vec(line);
    dir_vec.setLength(size);
    QPointF dir(dir_vec.dx(), dir_vec.dy());

    // normal vec (perpendicular to line)
    QLineF normal_vec(line.normalVector());
    normal_vec.setLength(swing);
    QPointF normal(normal_vec.dx(), normal_vec.dy());

    QPolygonF arrow_head;
    arrow_head << point << point - dir + normal << point - dir - normal;

    QPainterPath path;
    path.addPolygon(arrow_head);
    p.fillPath(path, p.pen().color());
}

QPointF DrawThings::nearest_box_corner(const QRectF &rc, const QPointF &p)
{
    double d1 = square_distance(rc.topLeft(), p);
    double d2 = square_distance(rc.topRight(), p);
    double d3 = square_distance(rc.bottomLeft(), p);
    double d4 = square_distance(rc.bottomRight(), p);
    if (d1 < d2) {
        if (d1 < d3) {
            if (d1 < d4) return rc.topLeft(); // d1
        } else {
            if (d3 < d4) return rc.bottomLeft(); // d3
        }
    } else {
        if (d2 < d3) {
            if (d2 < d4) return rc.topRight(); // d2
        } else {
            if (d3 < d4) return rc.bottomLeft(); // d3
        }
    }
    return rc.bottomRight(); // d4
}

void DrawThings::spline(QPainter &p, Agedge_t *edge,
                        double translatex, double translatey,
                        double scalex, double scaley)
{
#define XX(x) ((x + translatex) * scalex)
#define YY(y) ((y + translatey) * scaley)

    QPainterPath path;
    if((edge->u.spl->list!=0) && (edge->u.spl->list->size%3 == 1))
    {
        QPointF lastp;

        //If there is a starting point, draw a line from it to the first curve point
        if(edge->u.spl->list->sflag)
        {
            path.moveTo(XX(edge->u.spl->list->sp.x), YY(edge->u.spl->list->sp.y));
            path.lineTo(XX(edge->u.spl->list->list[0].x),
                    YY(edge->u.spl->list->list[0].y));
        }
        else {
            path.moveTo(XX(edge->u.spl->list->list[0].x),
                    YY(edge->u.spl->list->list[0].y));
        }
        lastp = path.currentPosition();

        //Loop over the curve points
        for(int i=1; i<edge->u.spl->list->size; i+=3) {
            lastp = path.currentPosition();
            path.cubicTo(XX(edge->u.spl->list->list[i].x),
                  YY(edge->u.spl->list->list[i].y),
                  XX(edge->u.spl->list->list[i+1].x),
                  YY(edge->u.spl->list->list[i+1].y),
                  XX(edge->u.spl->list->list[i+2].x),
                  YY(edge->u.spl->list->list[i+2].y));
        }

        //If there is an ending point, draw a line to it
        if(edge->u.spl->list->eflag) {
            path.lineTo(XX(edge->u.spl->list->ep.x), YY(edge->u.spl->list->ep.y));
        }

        p.drawPath(path);
        QPointF endp(XX(edge->tail->u.coord.x), YY(edge->tail->u.coord.y));
        arrow_head(p, QLineF(lastp, endp), endp, ARROW_SIZE, ARROW_SWING);
    }
#undef XX
#undef YY
}
