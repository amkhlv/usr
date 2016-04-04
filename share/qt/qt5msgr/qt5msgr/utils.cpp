#include "utils.h"
#include <QTime>
#include <QFileInfo>

void Utils::delay(int ms)
{
    QTime dieTime= QTime::currentTime().addMSecs(ms);
    while (QTime::currentTime() < dieTime)
    QCoreApplication::processEvents(QEventLoop::AllEvents, ms);
}

bool Utils::fileExists(QString path)
{
    QFileInfo checkFile(path);
    return checkFile.exists();
}
