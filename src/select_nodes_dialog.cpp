#include "select_nodes_dialog.h"
#include "ui_select_nodes_dialog.h"
#include "xref_node.h"

SelectNodesDialog::SelectNodesDialog(QWidget *parent, QList<xrefEditableNode *> nodes) :
    QDialog(parent),
    ui(new Ui::SelectNodesDialog)
{
    ui->setupUi(this);

    foreach(auto n, nodes) {
        // TODO: produce a warning?
        if (!n) continue;

        QListWidgetItem *listItem = new QListWidgetItem(n->m_name, ui->listWidget);
        listItem->setCheckState(Qt::Unchecked);
        ui->listWidget->addItem(listItem);
    }
}

SelectNodesDialog::~SelectNodesDialog()
{
    delete ui;
}
