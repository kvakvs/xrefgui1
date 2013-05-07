#ifndef GRAPH_CLASSES_H
#define GRAPH_CLASSES_H

#include <QSet>
#include <QString>
//#include <QPoint>
#include <QRect>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/labeled_graph.hpp>

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

//typedef boost::labeled_graph <
//    boost::adjacency_list <boost::vecS, boost::vecS, boost::directedS, xrefNode>,
//    QString> xrefGraph;
typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::directedS, xrefNode> xrefGraph;

//typedef boost::graph_traits<xrefGraph>::vertex_descriptor xrefVertex;

#endif // GRAPH_CLASSES_H
