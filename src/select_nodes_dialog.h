#ifndef SELECT_NODES_DIALOG_H
#define SELECT_NODES_DIALOG_H

#include <QList>
#include <QSet>
#include <QDialog>
#include <QDialogButtonBox>

class xrefSourceNode;
class QListWidgetItem;

namespace Ui {
class SelectNodesDialog;
}

class SelectNodesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit SelectNodesDialog(QWidget *parent, QList<QString> appnames,
                               QList<xrefSourceNode *> nodes,
                               QList<QString> selected_modules);
    ~SelectNodesDialog();

public:
    QSet<QString> m_selected_modules;

private:
    void select_all();
    void clear_all();

private slots:
    void on_buttonSelectAll_clicked();
    void on_buttonSelectNone_clicked();
    void on_buttonSave_clicked();
    void on_buttonCancel_clicked();
    void on_appsLW_itemChanged(QListWidgetItem *item);

private:
    Ui::SelectNodesDialog *ui;
    bool m_populating_now = false;
};

#endif // SELECT_NODES_DIALOG_H
