#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "utils.h"
#include <QFileSystemWatcher>
#include <QFileInfo>
#include <QUrl>
#include <QWebView>
#include <QDebug>
#include <QScrollBar>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    regexen(QList<QRegExp>()),
    knownFiles(QStringList())
{
    regexen << QRegExp(".*\\.html$") << QRegExp(".*\\.svg$");
    ui->setupUi(this);
    connect(ui->lineEdit, SIGNAL(returnPressed()), this, SLOT(handleUserTyped()));
    ui->lineEdit->setFocus();
}

void MainWindow::setIncomingFile(QString f)
{
    incomingFile = new QFile(f);
}

void MainWindow::setWatchedDir(QString dir)
{
    watchedDir = QDir(dir);
    knownFiles = watchedDir.entryList(QDir::Files);
    for (int i = 0; i < knownFiles.length(); i++) {
        QString f = knownFiles[i];
        if (doesMatchRegExp(f)) {
            qDebug() << "adding file to watch: " << watchedDir.filePath(f);
            watcher->addPath(watchedDir.filePath(f));
        }
    }
}

bool MainWindow::doesMatchRegExp(const QString &p)
{
    bool itMatches = false;
    for (int i=0; i < regexen.length(); i++) {
        if (regexen[i].exactMatch(p)) {
            itMatches = true;
        }
    }
    return itMatches;
}

bool MainWindow::couldOpen(const QString &path)
{
    int n = 5;
    while (n > 0 && !Utils::fileExists(path)) {
        n = n - 1 ;
        Utils::delay(200);
    }
    if (n > 0 ) {
        ui->webView->load(QUrl("file://" + QFileInfo(path).absoluteFilePath()));
        watcher->addPath(path);
        return true;
    } else { return false ; }
}

void MainWindow::handleFileChanged(const QString &filenamepath)
{
    qDebug() << "file changed: " << QFileInfo(filenamepath).absoluteFilePath() ;
    if (doesMatchRegExp(filenamepath)) {
        qDebug() << "and it matches our patterns";
        if (!couldOpen(filenamepath)) {
           qDebug() << "*** but we could not open it ***" ;
        }
    }
}

void MainWindow::handleDirectoryChanged(const QString &path)
{
    qDebug() << "-- watched files now: " << watcher->files();
    qDebug() << "directory changed: " << path;
    QStringList nowFiles = watchedDir.entryList(QDir::Files);
    for (int i = 0; i < nowFiles.length(); i++) {
        QString f = nowFiles[i];
        if (! knownFiles.contains(f)) {
            if (doesMatchRegExp(f)) {
                qDebug() << "detected new relevant file: " << watchedDir.filePath(f);
                watcher->addPath(watchedDir.filePath(f));
                knownFiles << f ;
                if (!couldOpen(watchedDir.filePath(f))) { qDebug() << "UNABLE TO OPEN FILE"; }
            }
        }
    }
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
