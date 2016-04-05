#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "utils.h"
#include <QFileSystemWatcher>
#include <QFileInfo>
#include <QUrl>
#include <QWebView>
#include <QScrollBar>
#include <QTextStream>
#include <QDateTime>
#include <cstdio>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    regexen(QList<QRegExp>()),
    logFile("qt5msgr.log"),
    watcher(new QFileSystemWatcher(QStringList()))
{
    logFile.open(QIODevice::WriteOnly | QIODevice::Append);
    logger = new QTextStream(&logFile);
    regexen << QRegExp(".*\\.html$") << QRegExp(".*\\.svg$");
    connect(watcher, SIGNAL(fileChanged(const QString&)), this, SLOT(handleFileChanged(const QString&)));
    ui->setupUi(this);
    connect(ui->lineEdit, SIGNAL(returnPressed()), this, SLOT(handleUserTyped()));
    ui->lineEdit->setFocus();
}

void MainWindow::setWatchedDir(QString dir)
{
    *logger << "=== STARTING at " << QDateTime::currentDateTime().toString("hh:mm:ss") << " ===" << endl;
    watchedDir = QDir(dir);
    QStringList fs = watchedDir.entryList(QDir::Files);
    for (int i = 0; i < fs.length(); i++) {
        QString f = fs[i];
        if (doesMatchRegExp(f)) {
            QString ff = watchedDir.filePath(f);
            *logger << "adding file to watch: " << ff << endl;
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
        *logger << "   >>> " << QFileInfo(path).absoluteFilePath() << " <<<" << endl;
        return true;
    } else {
        *logger << "*** FAILED TO OPEN " << path << " ***" << endl;
        qDebug()<< "*** FAILED TO OPEN " << path << " ***" << endl;
        //but re-add it anyway:
        watcher->addPath(path);
        return false ;
    }
}

void MainWindow::handleFileChanged(const QString &filenamepath)
{
    *logger << "file changed: " << filenamepath << endl;
    if (doesMatchRegExp(filenamepath)) {
        *logger << "and it matches our patterns" << endl;
        if (!couldOpen(filenamepath)) {
           *logger << "*** but we could not open it ***" << endl;
        } else {
           *logger << "refreshed view of : " << filenamepath << endl;
        }
    }
}

void MainWindow::handleDirectoryChanged(const QString &path)
{
    *logger << "directory changed: " << path << "\n";
    QStringList nowFiles = watchedDir.entryList(QDir::Files);
    QStringList watchlist = watcher->files();
    for (int i=0; i < watchlist.length(); i++) {
        *logger << " -- " << watchlist[i] ;
    }
    *logger << endl;
    for (int i = 0; i < nowFiles.length(); i++) {
        QString f = nowFiles[i];
        if (! watcher->files().contains(watchedDir.filePath(f))) {
            if (doesMatchRegExp(f)) {
                QString ff = watchedDir.filePath(f);
                if (couldOpen(ff)) {
                    watcher->addPath(ff);
                } else {
                    *logger << "*** PROBLEM: file " << ff << " LOST ***" << endl;
                    *output << "*** PROBLEM: file " << ff << " LOST ***" << endl;
                }
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
    *output << t << endl;
}

MainWindow::~MainWindow()
{
    delete watcher;
    delete logger;
    delete ui;
}
