import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    id: mainwin
    title: qsTr("localsite warning")

    signal putStr(string msg)
    signal killThread()
    onClosing: function() { if (control.dismiss == 0)  killThread(); }
    onAfterRendering: function() {width = box.width; height = box.height;}

    Item {
        id: control
        objectName: "controlObject"
        property int dismiss: 0
        onDismissChanged: mainwin.close();
    }
    Item {
        id: st
        objectName: "stateObject"

// ███████████████████████████████████████████████████████████████
// things below this line can be changed:

        property var warning: qsTr("")
        onWarningChanged: lbl.text = warning;
    }

    GroupBox {
        id: box
        GridLayout {
            columns: 1;
            Label {
                id: lbl;
                text: qsTr("---");
                font.family: "Courier"
                font.pointSize: 20;
            }

            Button {
                id: ok;
                text: qsTr("OK");
                onClicked: mainwin.close();
                font.family: "Courier";
                font.pointSize: 32;
            }
        }
    }
}
