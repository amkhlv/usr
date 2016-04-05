#include "mainwindow.h"
#include <QApplication>
#include <QFileSystemWatcher>
#include <QFile>
#include <QDebug>
#include <cstdio>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.output = new QTextStream(stdout);
    w.watcherForDir = new QFileSystemWatcher(QStringList(argv[1]));
    w.setWatchedDir(argv[1]);
    QObject::connect(w.watcherForDir, SIGNAL(directoryChanged(const QString&)), &w, SLOT(handleDirectoryChanged(const QString&)));
    w.show();
    int result = a.exec();
    return result;
}
