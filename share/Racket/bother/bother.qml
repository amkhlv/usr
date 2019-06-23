import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    title: qsTr("Reminder")

    id: mainwin
    signal putStr(string msg)
    signal killThread()
    onClosing: function() {
        putStr(JSON.stringify({command : "exit"}));
        if (control.dismiss == 0) {
            killThread();
        }
    }
    onAfterRendering: function() {
        width = box.width;
        height = box.height;
    }

    Item {
        id: control
        objectName: "controlObject"
        property int dismiss: 0
        onDismissChanged: mainwin.close();
    }
    Item {
        id: st
        objectName: "stateObject"
        property string init: "";
        property var todolist: [];
        onInitChanged: function () { putStr(JSON.stringify({status: "ready"})); }
        onTodolistChanged: function () {
            todos.model = todolist;
            //putStr("OK");
        }
    }
    Item {
        Timer {
            id: raiser
            interval: 180000; running: true; repeat: true
            onTriggered: mainwin.raise()
        }
    }

    GroupBox {
        id: box
        GridLayout {
            columns: 1;
            Repeater {
                id: todos;
                model: []
                Button {
                    text: modelData.nick
                    Layout.fillWidth: true
                    contentItem: Rectangle {
                        color: modelData.urgent ? "#c00000" : "#ffffff"
                        Text {
                            text: modelData.nick
                            color: modelData.urgent ? "#ffffff" : "#0000bb"
                            font.pointSize: 18
                        }
                        width: childrenRect.width
                        height: childrenRect.height
                    }
                    function activate() {
                        entry.text = modelData.nick
                        entry.orig = modelData.nick
                        entry.focus = true
                        entry.selectAll()
                    }

                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                }
            }
            TextField {
                id: entry
                property string orig: ""
                text: ""
                Layout.fillWidth: true
                selectionColor: "#880000"
                font.pointSize: 18
                onEditingFinished: function() {
                    if (orig == "") {
                        if (text.length > 0) {
                            putStr(JSON.stringify({add : text}));
                        }
                    } else {
                        if (text == "") {
                            putStr(JSON.stringify({del : orig}));
                        } else {
                            putStr(JSON.stringify({repl : [orig , text]}))
                        }
                        orig = "";
                    }
                    text = "";
                }
            }
            GridLayout {
                rows: 1;
                Button {
                    text: "10 min" ;
                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                    function activate() {
                        putStr(JSON.stringify({snooze : 10}));
                        mainwin.close();
                    }
                }
                Button {
                    text: "30 min" ;
                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                    function activate() {
                        putStr(JSON.stringify({snooze : 30}));
                        mainwin.close();
                    }
                }
                Button {
                    text: "1 hour" ;
                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                    function activate() {
                        putStr(JSON.stringify({snooze : 60}));
                        mainwin.close();
                    }
                }
                Button {
                    text: "3 hours" ;
                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                    function activate() {
                        putStr(JSON.stringify({snooze : 180}));
                        mainwin.close();
                    }
                }
                Button {
                    text: "Exit" ;
                    onPressed: activate()
                    Keys.onReturnPressed: activate()
                    function activate() {
                        mainwin.close();
                    }
                }
            }
        }
    }
}
