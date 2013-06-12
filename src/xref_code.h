#ifndef XREF_CODE_PARTS_H
#define XREF_CODE_PARTS_H

#include <QString>
#include <QList>

class xrefCApp
{
public:
    xrefCApp() {}

public:
    QString m_name;
};

class xrefCMod
{
public:
    xrefCMod() {}

public:
    QString m_name;
    xrefCApp * m_app;
};

class xrefCFun
{
public:
    xrefCFun() {}

public:
    /// Short function name
    QString m_name;
    /// Full function name in form of "module:function/arity"
    QString m_full_name;
    xrefCMod * m_module;
    xrefCApp * m_app;

    QList<xrefCFun *> m_callees;
};

#endif // XREF_CODE_PARTS_H
