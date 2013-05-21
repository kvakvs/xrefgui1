#include <QListWidgetItem>

#include "select_nodes_dialog.h"
#include "ui_select_nodes_dialog.h"
#include "xref_node.h"

SelectNodesDialog::SelectNodesDialog(QWidget *parent,
                                     QList<QString> appnames,
                                     QList<xrefSourceNode *> nodes) :
    QDialog(parent),
    ui(new Ui::SelectNodesDialog)
{
    ui->setupUi(this);

    foreach(xrefSourceNode * n, nodes) {
        // TODO: produce a warning?
        if (!n) continue;

        // app name prefix is used when selecting modules belonging to that app
        QString item_name = QString("%1: %2").arg(n->m_app_name).arg(n->m_name);
        QListWidgetItem *listItem = new QListWidgetItem(item_name, ui->modulesLW);
        listItem->setCheckState(Qt::Unchecked);
        ui->modulesLW->addItem(listItem);
    }
    ui->modulesLW->sortItems();

    foreach(auto appname, appnames) {
        QListWidgetItem *listItem = new QListWidgetItem(appname, ui->appsLW);
        listItem->setCheckState(Qt::Unchecked);
        ui->appsLW->addItem(listItem);
    }
    ui->appsLW->sortItems();
}

SelectNodesDialog::~SelectNodesDialog()
{
    delete ui;
}

void SelectNodesDialog::select_all()
{
    // select modules
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        i->setCheckState(Qt::Checked);
    }
    // do the same for apps
    for(int row = 0; row < ui->appsLW->count(); row++) {
        QListWidgetItem * i = ui->appsLW->item(row);
        i->setCheckState(Qt::Checked);
    }
}

void SelectNodesDialog::clear_all()
{
    // clear selection from modules
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        i->setCheckState(Qt::Unchecked);
    }
    // do the same for apps
    for(int row = 0; row < ui->appsLW->count(); row++) {
        QListWidgetItem * i = ui->appsLW->item(row);
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
    m_selected_modules.clear();
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        if (i->checkState() == Qt::Checked) {
            m_selected_modules.insert(i->text().split(": ").back());
        }
    }
    accept();
}

void SelectNodesDialog::on_buttonCancel_clicked()
{
    reject();
}

void SelectNodesDialog::on_appsLW_itemChanged(QListWidgetItem *item)
{
    auto check_state = item->checkState();

    // select modules by app prefix
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        if (i->text().startsWith(item->text() + ": ")) {
            i->setCheckState(check_state);
        }
    }
}
