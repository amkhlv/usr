#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFile>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    void setIncomingFile(QString f);
    ~MainWindow();

public slots:
    void handleFileChanged(const QString & path);
    void handleUserTyped();

private:
    QFile* incomingFile;
    Ui::MainWindow *ui;
};

#endif // MAINWINDOW_H
