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
    graph.cpp \
    render_things.cpp

HEADERS  += main_window.h \
    graph_render_widget.h \
    graph.h \
    render_things.h

FORMS    += main_window.ui
