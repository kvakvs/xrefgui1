#-------------------------------------------------
#
# Project created by QtCreator 2013-05-07T17:00:15
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = xrefgui
TEMPLATE = app
CONFIG += c++11

linux:LIBS += -lgvc -lgraph

SOURCES += main.cpp\
        main_window.cpp \
    graph_render_widget.cpp \
    render_things.cpp \
    xref_node.cpp \
    graphviz_graph.cpp \
    xref_edge.cpp \
    xref_graph.cpp \
    select_nodes_dialog.cpp \
    xref_code.cpp

HEADERS  += main_window.h \
    graph_render_widget.h \
    render_things.h \
    xref_node.h \
    graphviz_graph.h \
    xref_edge.h \
    xref_graph.h \
    select_nodes_dialog.h \
    xref_code.h

FORMS    += main_window.ui \
    select_nodes_dialog.ui

RESOURCES +=

INCLUDEPATH += ../qtpropertybrowser/src
include(../qtpropertybrowser/src/qtpropertybrowser.pri)
