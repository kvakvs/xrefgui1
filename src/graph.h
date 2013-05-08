#ifndef GRAPH_CLASSES_H
#define GRAPH_CLASSES_H

#include <QSet>
#include <QString>
#include <QRect>
#include <QPoint>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/labeled_graph.hpp>
#include <boost/graph/topology.hpp>

class xrefNode
{
public:
    xrefNode();
    xrefNode(const QString & name);

    QString m_name;
//    QSet<QString> m_edges_out;
//    QSet<QString> m_edges_in; // probably this is extraneous and remove it later
    QRect m_rect;
};

enum vertex_position_t { vertex_position };
namespace boost { BOOST_INSTALL_PROPERTY(vertex, position); }

typedef boost::rectangle_topology<> xref_topology_type;
typedef xref_topology_type::point_type xref_point_type;

//typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::directedS, xrefNode> xrefGraph;
typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::directedS,
    // Vertex properties
    boost::property<boost::vertex_index_t, unsigned long int
        , boost::property<vertex_position_t, xref_point_type> >,
    // Edge properties
    boost::property<boost::edge_weight_t, double>
    > xrefGraph;

#endif // GRAPH_CLASSES_H
