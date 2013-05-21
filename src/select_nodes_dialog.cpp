
#include "select_nodes_dialog.h"
#include "ui_select_nodes_dialog.h"
#include "xref_node.h"

SelectNodesDialog::SelectNodesDialog(QWidget *parent,
                                     QList<QString> appnames,
                                     QList<xrefEditableNode *> nodes) :
    QDialog(parent),
    ui(new Ui::SelectNodesDialog)
{
    ui->setupUi(this);

    foreach(xrefEditableNode * n, nodes) {
        // TODO: produce a warning?
        if (!n) continue;

        QString item_name = QString("%1: %2").arg(n->m_app_name).arg(n->m_name);
        QListWidgetItem *listItem = new QListWidgetItem(item_name, ui->modulesLW);
        if (n->m_editor_flags.show) {
            listItem->setCheckState(Qt::Checked);
        } else {
            listItem->setCheckState(Qt::Unchecked);
        }
        ui->modulesLW->addItem(listItem);
    }

    foreach(auto appname, appnames) {
        QListWidgetItem *listItem = new QListWidgetItem(appname, ui->appsLW);
        listItem->setCheckState(Qt::Unchecked);
        ui->appsLW->addItem(listItem);
    }
}

SelectNodesDialog::~SelectNodesDialog()
{
    delete ui;
}

void SelectNodesDialog::select_all()
{
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        i->setCheckState(Qt::Checked);
    }
}

void SelectNodesDialog::clear_all()
{
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        i->setCheckState(Qt::Unchecked);
    }
}

void SelectNodesDialog::on_buttonSelectAll_clicked()
{
    select_all();
}

void SelectNodesDialog::on_buttonSelectNone_clicked()
{
    clear_all();
}

void SelectNodesDialog::on_buttonSave_clicked()
{
    accept();
}

void SelectNodesDialog::on_buttonCancel_clicked()
{
    reject();
}
