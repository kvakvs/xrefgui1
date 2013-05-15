#ifndef SELECT_NODES_DIALOG_H
#define SELECT_NODES_DIALOG_H

#include <QList>
#include <QDialog>

class xrefEditableNode;

namespace Ui {
class SelectNodesDialog;
}

class SelectNodesDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit SelectNodesDialog(QWidget *parent, QList<xrefEditableNode *> nodes);
    ~SelectNodesDialog();
    
private:
    Ui::SelectNodesDialog *ui;
};

#endif // SELECT_NODES_DIALOG_H
