#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFile>
#include <QFileSystemWatcher>
#include <QDir>
#include <QTextStream>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    void setIncomingFile(QString f);
    QFileSystemWatcher *watcher;
    void setWatchedDir(QString dir);
    QFile logFile;
    QTextStream* logger;
    ~MainWindow();

public slots:
    void handleFileChanged(const QString & path);
    void handleDirectoryChanged(const QString & path);
    void handleUserTyped();

private:
    QStringList knownFiles;
    QDir watchedDir;
    bool doesMatchRegExp(const QString & p);
    bool couldOpen(const QString & path);
    QFile* incomingFile;
    Ui::MainWindow *ui;
    QList<QRegExp> regexen;
};

#endif // MAINWINDOW_H
