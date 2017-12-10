import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {
    visible: true
    title: qsTr("GStreams")

// █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
// do not change below until solid line

    id: mainwin
    signal putStr(string msg)
    signal killThread()
    onClosing: function() { if (control.dismiss == 0)  killThread(); }
    onAfterRendering: function () {width = clmn.width ; height = clmn.height }

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

        property var xs: [] ;
        // [{"name": "MyFirstStream", "isActive": 0}, ...]
        onXsChanged: function () {
            progs.model = xs;
        }
    }

    Column {
        id:	clmn;
        Repeater {
            id: progs;
            model: []
            Button {
                text: modelData.name;
                onClicked: mainwin.putStr(modelData.name);
                font.pointSize: 20;
                background: Rectangle {
                    color:  (modelData.isActive == 1) ? "lightyellow" : "lightblue";
                }
            }
        }
    }
}
