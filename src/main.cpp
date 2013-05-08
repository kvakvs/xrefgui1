#include "main_window.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QCoreApplication::setOrganizationName("Erlang Solutions");
    QCoreApplication::setOrganizationDomain("erlang-solutions.com");
    QCoreApplication::setApplicationName("xrefGUI");

    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    return a.exec();
}
