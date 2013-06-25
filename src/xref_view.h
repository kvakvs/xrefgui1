#ifndef XREF_VIEW_H
#define XREF_VIEW_H

#include <QGraphicsView>

class QGraphicsScene;
class QGraphicsItem;
class QAction;
class QActionGroup;
class QContextMenuEvent;

class xrefSceneNode_Module;

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
    void contextMenuEvent(QContextMenuEvent *event);

private:
    /// Menu item group to select between fun and module level of detail
    QActionGroup * m_agroup_detail;
    /// Menu item to select mod level of detail
    QAction * m_action_modlevel;
    /// Menu item to select fun level of detail
    QAction * m_action_funlevel;

private:
    /// Creates popup menu for right clicking on Module scene node
    void open_context_menu_module(QContextMenuEvent *event, QGraphicsItem * i);

    /// Callback: When user clicks popup menu on module
    void show_module_functions(xrefSceneNode_Module * node, bool show);
    /// Callback: When user clicks popup menu on module
    void show_callees(xrefSceneNode_Module * node);
    /// Callback: When user clicks popup menu on module
    void hide_module(xrefSceneNode_Module * node);
};

#endif // XREF_VIEW_H
