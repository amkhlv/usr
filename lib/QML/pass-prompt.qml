import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    id: mainwin
    title: qsTr("Approval Request")

    signal putStr(string msg)
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

    }

    GroupBox {
        id: box
        GridLayout {
            columns: 1;
            Label {
                id: lbl;
                text: "enter passphrase"
                font.pointSize: 32
                font.family: "Courier"
            }
            TextField {
                Layout.fillWidth: true
                echoMode: TextInput.Password;
                onAccepted: putStr(text);
            }
        }
    }
}
