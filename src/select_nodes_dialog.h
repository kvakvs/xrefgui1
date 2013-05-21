#ifndef SELECT_NODES_DIALOG_H
#define SELECT_NODES_DIALOG_H

#include <QList>
#include <QDialog>
#include <QDialogButtonBox>

class xrefEditableNode;

namespace Ui {
class SelectNodesDialog;
}

class SelectNodesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SelectNodesDialog(QWidget *parent, QList<QString> appnames,
                               QList<xrefEditableNode *> nodes);
    ~SelectNodesDialog();

private:
    void select_all();
    void clear_all();

private slots:
    void on_buttonSelectAll_clicked();

    void on_buttonSelectNone_clicked();

private:
    Ui::SelectNodesDialog *ui;
};

#endif // SELECT_NODES_DIALOG_H
