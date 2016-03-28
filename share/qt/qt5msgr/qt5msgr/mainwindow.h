#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFile>
#include <QFileSystemWatcher>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    void setIncomingFile(QString f);
    QFileSystemWatcher* watch;
    ~MainWindow();

public slots:
    void handleFileChanged(const QString & path);
    void handleUserTyped();

private:
    QFile* incomingFile;
    Ui::MainWindow *ui;
    void delay();
};

#endif // MAINWINDOW_H
