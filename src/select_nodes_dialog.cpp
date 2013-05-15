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

        QListWidgetItem *listItem = new QListWidgetItem(n->m_name, ui->modulesLW);
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
