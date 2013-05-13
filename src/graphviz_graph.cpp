#include "graphviz_graph.h"
#include "main_window.h"

// Directly use agsafeset which always works, contrarily to agset
//int _agset(void * object, const QString & attr, const QString & value)
//{
//    return agsafeset(object, const_cast<char *>(qPrintable(attr)),
//                     const_cast<char *>(qPrintable(value)),
//                     const_cast<char *>(qPrintable(value)));
//}

//xrefGraphvizGraph::xrefGraphvizGraph()
//{
//    m_gvc = gvContext();
//    m_graph = agopen("xrefgraph", AGDIGRAPH/*STRICT*/);
//    //_agset(m_graph, "splines", "false");
//}

//xrefGraphvizGraph::~xrefGraphvizGraph()
//{
//    agclose(m_graph);
//    gvFreeContext(m_gvc);
//}

//void xrefGraphvizGraph::redo_layout(const char * algo) {
//    if (algo != nullptr) {
//        m_last_layout = algo;
//    } else {
//        algo = m_last_layout;
//    }

//    auto main = MainWindow::m_singleton;
//    bool use_spline = main->m_settings.value("layout/spline", false).toBool();

//    if (use_spline) { _agset(m_graph, "splines", "true"); }
//    else { _agset(m_graph, "splines", "false"); }

//    gvFreeLayout(m_gvc, m_graph);
//    gvLayout(m_gvc, m_graph, algo);

//    main->update();
//}
