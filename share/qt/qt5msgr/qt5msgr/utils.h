#ifndef UTILS_H
#define UTILS_H

#include <QApplication>
#include <QString>

namespace Utils
{
    void delay(int ms);
    bool fileExists(QString path);
};

#endif // UTILS_H
