#ifndef RENDER_THINGS_H
#define RENDER_THINGS_H

#include <QPainter>
#include <QPoint>

#include <graphviz/gvc.h>

class DrawThings
{
public:
    // Draws line from p1 to p2, but skips first 'offset' pixels
    static void line_with_offset_from_p1(QPainter & p, const QPointF & p1,
                                         const QPointF & p2, double offset);
    // Draws line with arrowhead
    static void arrow(QPainter & p, const QPointF & p1, const QPointF & p2);

    // Finds box corner closest to given point p
    static QPointF nearest_box_corner(const QRectF & rc, const QPointF & p);

    static void spline(QPainter & p, Agedge_t * edge,
                       double translatex, double translatey,
                       double scalex, double scaley);

    static void arrow_head(QPainter &p, const QLineF & line, const QPointF &point,
                           double size, double swing);

private:
    static inline double square_distance(const QPointF & p1, const QPointF & p2)
    {
        auto d = p1 - p2;
        return d.x() * d.x() + d.y() * d.y();
    }
};

#endif // RENDER_THINGS_H
