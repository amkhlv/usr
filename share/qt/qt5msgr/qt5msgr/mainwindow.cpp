#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "utils.h"
#include <QFileSystemWatcher>
#include <QFileInfo>
#include <QUrl>
#include <QWebView>
#include <QScrollBar>
#include <QTextStream>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    regexen(QList<QRegExp>()),
    knownFiles(QStringList()),
    logFile("qt5msgr.log")
{
    logFile.open(QIODevice::WriteOnly | QIODevice::Append);
    logger = new QTextStream(&logFile);
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
    *logger << "=== STARTING ===" << endl;
    watchedDir = QDir(dir);
    knownFiles = watchedDir.entryList(QDir::Files);
    for (int i = 0; i < knownFiles.length(); i++) {
        QString f = knownFiles[i];
        if (doesMatchRegExp(f)) {
            *logger << "adding file to watch: " << f << endl;
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
    for (int i = 0; i < nowFiles.length(); i++) {
        QString f = nowFiles[i];
        if (! knownFiles.contains(f)) {
            if (doesMatchRegExp(f)) {
                watcher->addPath(watchedDir.filePath(f));
                knownFiles << f ;
                if (!couldOpen(watchedDir.filePath(f))) {
                    *logger << "******** UNABLE TO OPEN FILE ********" << endl;
                } else {
                    *logger << "refreshed view of : " << f << endl;
                }
            }
        }
    }
    QStringList watchlist = watcher->files();
    for (int i=0; i < watchlist.length(); i++) {
        *logger << " -- " << watchlist[i] ;
    }
    *logger << endl;
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
    delete logger;
    delete ui;
    delete incomingFile;
}
