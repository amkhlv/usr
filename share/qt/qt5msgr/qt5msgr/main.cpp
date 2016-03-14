#include "mainwindow.h"
#include <QApplication>
#include <QFileSystemWatcher>
#include <QFile>
#include <QDebug>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.setIncomingFile(argv[2]);
    QFileSystemWatcher watcher;
    watcher.addPath(argv[1]);
    QObject::connect(&watcher, SIGNAL(fileChanged(const QString&)), &w, SLOT(handleFileChanged(const QString&)));
    w.show();
    int result = a.exec();
    return result;
}
