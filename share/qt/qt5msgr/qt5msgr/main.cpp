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
    w.watcher = new QFileSystemWatcher(QStringList(argv[1]));
    w.setWatchedDir(argv[1]);
    QObject::connect(w.watcher, SIGNAL(fileChanged(const QString&)), &w, SLOT(handleFileChanged(const QString&)));
    QObject::connect(w.watcher, SIGNAL(directoryChanged(const QString&)), &w, SLOT(handleDirectoryChanged(const QString&)));
    w.show();
    int result = a.exec();
    return result;
}
