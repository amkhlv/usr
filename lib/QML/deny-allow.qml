import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls.Styles 1.4

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

        property var question: qsTr("")
        onQuestionChanged: lbl.text = qsTr(question);
    }

    GroupBox {
        id: box
        GridLayout {
            columns: 2;
            Label {
                id: lbl;
                Layout.columnSpan: 2;
                text: qsTr("deny or allow?");
                font.family: "Courier"
                anchors.fill: parent;
                horizontalAlignment: Text.AlignHCenter;
                font.pointSize: 20;
                renderType: Text.NativeRendering;
            }
            Button {
                id: buttonDeny;
                text: qsTr("deny");
                font.family: "Courier";
                font.pointSize: 32;
                onClicked: mainwin.putStr("deny");
            }
            Button {
                id: buttonAllow;
                text: qsTr("allow");
                font.family: "Courier";
                font.pointSize: 32;
                onClicked: mainwin.putStr("allow");
                background: Rectangle {
                    color: "yellow";
                }
            }
        }
    }
}
