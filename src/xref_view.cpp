#include <QWheelEvent>
#include <QApplication>
#include <QMenu>
#include <QAction>
#include <QActionGroup>
#include <QGraphicsItem>

#include "xref_node.h"
#include "xref_view.h"

xrefView::xrefView(QWidget *parent) :
    QGraphicsView(parent)
{
}

xrefView::xrefView(QGraphicsScene * s, QWidget *parent) :
    QGraphicsView(s, parent)
{
}

void xrefView::wheelEvent(QWheelEvent *event)
{
    static const double ZOOMQ = 1.03;
    if (QApplication::keyboardModifiers() & Qt::ControlModifier) {
        auto d = event->delta();
        if (d < 0) {
            QGraphicsView::scale(1.0 / ZOOMQ, 1.0 / ZOOMQ);
        } else if (d > 0) {
            QGraphicsView::scale(ZOOMQ, ZOOMQ);
        }
    }
}

void xrefView::contextMenuEvent(QContextMenuEvent *event)
{
    auto scene_pos = this->mapFromGlobal(event->globalPos());
    QGraphicsItem * i = this->itemAt(scene_pos.x(), scene_pos.y());

    // if we targeted empty space
    if (! i) {
        QMenu menu(this);
        menu.addAction(new QAction("fgsfds", this));
        menu.exec(event->globalPos());
        event->accept();
        return;
    }

    // if we targeted module node
    if (i->type() == xrefNodeType_Module) {
        open_context_menu_module(event, i);
        event->accept();
        return;
    }

    // if we targeted function node

    // if we targeted an edge

    // if all else fails
    event->ignore();
}

void xrefView::open_context_menu_module(QContextMenuEvent *event, QGraphicsItem * i)
{
    QMenu menu(this);
    auto node = dynamic_cast <xrefSceneNode_Module *> (i);
    if (! node) return; // no menu if that's not a module scenenode

    auto title = new QAction(tr("Module: %1").arg(node->m_name), this);
    title->setEnabled(false);
    menu.addAction(title);
    menu.addSeparator();

    auto a_modlevel = new QAction(tr("&Function Detail Level"), this);
    a_modlevel->setStatusTip(tr("Unfold module node to show functions"));
    QObject::connect(a_modlevel, & QAction::triggered,
                     [this,node]() { this->show_module_functions(node, true); });
    menu.addAction(a_modlevel);

    auto a_funlevel = new QAction(tr("&Module Detail Level"), this);
    a_funlevel->setStatusTip(tr("Fold module node to show module only, not functions"));
    QObject::connect(a_funlevel, & QAction::triggered,
                     [this,node]() { this->show_module_functions(node, false); });
    a_funlevel->setChecked(true);
    menu.addAction(a_funlevel);

    menu.addSeparator();

    auto a_show_callees = new QAction(tr("Unfold &Callees"), this);
    a_show_callees->setStatusTip(tr("Unfold modules referred to by this module"));
    QObject::connect(a_show_callees, & QAction::triggered,
                     [this,node]() { this->show_callees(node); });
    menu.addAction(a_show_callees);

    auto a_hide_mod = new QAction(tr("&Hide Module"), this);
    a_hide_mod->setStatusTip(tr("Hide selected module(s)"));
    QObject::connect(a_hide_mod, & QAction::triggered,
                     [this,node]() { this->hide_module(node); });
    menu.addAction(a_hide_mod);

    menu.exec(event->globalPos());
}

void xrefView::show_module_functions(xrefSceneNode_Module *node, bool show)
{
}

void xrefView::show_callees(xrefSceneNode_Module *node)
{
}

void xrefView::hide_module(xrefSceneNode_Module *node)
{
}

