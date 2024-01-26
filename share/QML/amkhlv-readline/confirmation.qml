import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    title: qsTr("confirm deletion")

// █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
// do not change below until solid line

    id: mainwin
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

        property var login: "";
        onLoginChanged: loginField.text = login;
        property var changedOn: "";
        onChangedOnChanged: changedOnField.text = changedOn;
        property var expiringOn: "";
        onExpiringOnChanged: expiringOnField.text = expiringOn;
        property var description: "";
        onDescriptionChanged: descriptionField.text = description;
        property var notes: "";
        onNotesChanged: notesField.text = notes;
        property var loginChallenge: "";
        onLoginChallengeChanged: loginChallengeField.text = loginChallenge;
        property var forgotPasswordChallenge: "";
        onForgotPasswordChallengeChanged: forgotPasswordChallengeField.text = forgotPasswordChallenge;
        property var secretNotes: "";
        onSecretNotesChanged: secretNotesField.text = secretNotes;
        property var tags: [];
        onTagsChanged: function() {
            if (tags.length > 0) {
            var tagstring = tags[0];
            for (var i = 1; i < tags.length - 1; i++) {
                tagstring = tagstring + "," + tags[i]
            }
            tagsField.text = tagstring
            }
        }
    }

    GroupBox {
        id: box
        focus: true
        Keys.onPressed: {
            if (event.key == Qt.Key_Escape) {
                mainwin.close()
            }
        }
        GridLayout {
            columns: 2;
            Label{ text: "login" }
            Label {
                id: loginField
                Layout.fillWidth: true
            }
            Label { text: "changed on" }
            Label {
                id: changedOnField
                Layout.fillWidth: true
            }
            Label { text: "expiring on" }
            Label {
                id: expiringOnField
                Layout.fillWidth: true
            }
            Label { text: "description" }
            Label {
                id: descriptionField
                Layout.fillWidth: true
            }
            Label { text: "notes" }
            Label{
                id: notesField
                Layout.fillWidth: true
            }
            Label { text: "login challenge" }
            Label {
                id: loginChallengeField
                Layout.fillWidth: true
            }
            Label { text: "forgot passwd challenge" }
            Label {
                id: forgotPasswordChallengeField
                Layout.fillWidth: true
            }
            Label { text: "secret notes" }
            Label{
                id: secretNotesField
                Layout.fillWidth: true
            }
            Label { text: "tags" }
            Label {
                id: tagsField
                Layout.fillWidth: true
            }
            Label { text: "delete account?" }
            RowLayout {
                Button {
                    id: btnYes
                    text: "yes"
                    onClicked: function() {
                        putStr("yes")
                        mainwin.close()
                    }
                }
                Button {
                    id: btnNo
                    text: "no"
                    onClicked: function() {
                        putStr("no")
                        mainwin.close()
                    }
                }
            }
        }
    }
}
