#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QFileSystemWatcher>
#include <QFileInfo>
#include <QUrl>
#include <QWebView>
#include <QDebug>
#include <QScrollBar>
#include <QTime>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    connect(ui->lineEdit, SIGNAL(returnPressed()), this, SLOT(handleUserTyped()));
    ui->lineEdit->setFocus();
}

void MainWindow::setIncomingFile(QString f)
{
    incomingFile = new QFile(f);
}

void MainWindow::delay()
{
    QTime dieTime= QTime::currentTime().addMSecs(100);
    while (QTime::currentTime() < dieTime)
        QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
}

void MainWindow::handleFileChanged(const QString &path)
{
    qDebug() << "file changed: " << QFileInfo(path).absoluteFilePath() ;
    QFileInfo checkFile(path);
    while(!checkFile.exists()) delay();
    watch->addPath(path);
    ui->webView->load(QUrl("file://" + QFileInfo(path).absoluteFilePath()));
}

void MainWindow::handleUserTyped()
{
    QString t = ui->lineEdit->text();
    ui->textBrowser->append(t);
    QScrollBar *sb = ui->textBrowser->verticalScrollBar();
    sb->setValue(sb->maximum());
    ui->lineEdit->setText("");
    QByteArray utf8 ;
    utf8.append(t + "\n");
    incomingFile->open(QIODevice::Append);
    incomingFile->write(utf8);
    incomingFile->close();
}

MainWindow::~MainWindow()
{
    delete ui;
    delete incomingFile;
}
