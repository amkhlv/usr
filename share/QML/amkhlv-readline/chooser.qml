import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    title: qsTr("Passphrase entry")

// █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
// do not change below until solid line

    id: mainwin
    signal putStr(string msg)
    signal killThread()
    onClosing: function() { if (control.dismiss == 0)  killThread(); }
    color: "#503030";

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

        property var options: [];
        property string colorWindow: "#000020";
        onColorWindowChanged: function() {
            mainwin.color = colorWindow;
        }

        onOptionsChanged: function() {
            for (var i = 0; i < options.length ; i++) {
                choices.model.append({
                                         "ind": i,
                                         "txt": options[i]["txt"],
                                         "bgIndex": options[i]["bgIndex"],
                                         "fgIndex": options[i]["fgIndex"],
                                         "bgChoice": options[i]["bgChoice"],
                                         "fgChoice": options[i]["fgChoice"]
                                     })
            }
        }
    }


    GridLayout {
        id: vbox
        columns: 1;
        focus: true
        columnSpacing: 1;
        rowSpacing: 2;
        Keys.onPressed: {
            if (event.key === Qt.Key_Escape) {
                mainwin.close()
            } else {
                putStr(event.key - Qt.Key_0)
                mainwin.close()
            }
        }
        Repeater {
            id: choices
            model: ListModel {}
            delegate:  RowLayout {
                Label {
                    text: ind
                    color: fgIndex
                    font.pointSize: 14
                    font.bold: true
                    background: Rectangle { 
                        color: bgIndex
                        radius: 5
                    }
                }
                Label {
                    text: txt
                    color: fgChoice
                    font.pointSize: 14
                    font.family: "Monospace"
                    background: Rectangle { color: bgChoice }
                }
            }
        }
    }
}
