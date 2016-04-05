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
    QFileSystemWatcher *watcher;
    void setWatchedDir(QString dir);
    QFile logFile;
    QTextStream* logger;
    QTextStream* output;
    ~MainWindow();

public slots:
    void handleFileChanged(const QString & path);
    void handleDirectoryChanged(const QString & path);
    void handleUserTyped();

private:
    QDir watchedDir;
    bool doesMatchRegExp(const QString & p);
    bool couldOpen(const QString & path);
    Ui::MainWindow *ui;
    QList<QRegExp> regexen;
};

#endif // MAINWINDOW_H
