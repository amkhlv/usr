import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3
import QtQuick.Window 2.0

ApplicationWindow {

    visible: true
    id: mainwin
    title: qsTr("One Line Entry")

    signal putStr(string msg)

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

        property string prompt: ""
        onPromptChanged: lbl.text = prompt
        property string winttl: ""
        onWinttlChanged: mainwin.title = winttl
        property int w: 0
        onWChanged: function () {
            lt.width = w
            mainwin.width = w
            mainwin.height = lt.height
            mainwin.setX(Screen.width/2 - mainwin.width / 2)
            mainwin.setY(Screen.height/2 - mainwin.height / 2)
        }
        property int lblfontsize: 0
        onLblfontsizeChanged: function () { lbl.font.pointSize = lblfontsize ; btn.font.pointSize = lblfontsize; }
        property int entryfontsize: 0
        onEntryfontsizeChanged: txtfield.font.pointSize = entryfontsize

    }

        GridLayout {
            id: lt;
            columns: 1;
            Layout.fillWidth: true
            Label {
                id: lbl;
                text: "enter below:"
                font.family: "Courier"
            }
            TextField {
                id: txtfield
                Layout.fillWidth: true
                onAccepted: function () { putStr(text); mainwin.close(); }
            }
            Button {
                id: btn;
                text: "Enter"
                onClicked: function () { putStr(txtfield.text) ; mainwin.close() ; }
            }
        }
    }
