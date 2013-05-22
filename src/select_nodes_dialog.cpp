#include <QListWidgetItem>

#include "select_nodes_dialog.h"
#include "ui_select_nodes_dialog.h"
#include "xref_node.h"

SelectNodesDialog::SelectNodesDialog(
        QWidget *parent, QList<QString> appnames,
        QList<xrefSourceNode *> nodes,
        QList<QString> selected_modules)
    : QDialog(parent)
    , ui(new Ui::SelectNodesDialog)
{
    ui->setupUi(this);
    QSet<QString> appnames_in_module_list;

    m_populating_now = true;
    // populate modules list
    foreach(xrefSourceNode * n, nodes) {
        // TODO: produce a warning?
        if (!n) continue;

        // app name prefix is used when selecting modules belonging to that app
        QString item_name = QString("%1: %2").arg(n->m_app_name).arg(n->m_name);
        QListWidgetItem *listItem = new QListWidgetItem(item_name, ui->modulesLW);
        if (selected_modules.contains(n->m_name)) {
            listItem->setCheckState(Qt::Checked);
            // save app name of every module, to set checks in apps list below
            appnames_in_module_list.insert(n->m_app_name);
        } else {
            listItem->setCheckState(Qt::Unchecked);
        }
        // add to widget
        ui->modulesLW->addItem(listItem);
    }
    ui->modulesLW->sortItems();

    // Populate applications list
    foreach(auto appname, appnames) {
        QListWidgetItem *listItem = new QListWidgetItem(appname, ui->appsLW);
        if (appnames_in_module_list.contains(appname)) {
            listItem->setCheckState(Qt::Checked);
        } else {
            listItem->setCheckState(Qt::Unchecked);
        }
        // add to widget
        ui->appsLW->addItem(listItem);
    }
    ui->appsLW->sortItems();
    m_populating_now = false;
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
    if (m_populating_now) { return; }
    auto check_state = item->checkState();

    // select modules by app prefix
    for(int row = 0; row < ui->modulesLW->count(); row++) {
        QListWidgetItem * i = ui->modulesLW->item(row);
        if (i->text().startsWith(item->text() + ": ")) {
            i->setCheckState(check_state);
        }
    }
}
