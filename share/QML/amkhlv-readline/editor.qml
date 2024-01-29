import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

ApplicationWindow {

    visible: true
    width: 400
    height: 200
    title: qsTr("Edit account data")

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
        property var password: "";
        onPasswordChanged: passwordField.text = password;
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
            for (var i = 1; i < tags.length ; i++) {
                tagstring = tagstring + "," + tags[i]
            }
            tagsField.text = tagstring
            }
        }
    }

    GroupBox {
        id: box
        GridLayout {
            columns: 2;
            Label{ text: "login" }
            TextField {
                id: loginField
                Layout.fillWidth: true
            }
            Label { text: "password" }
            TextField {
                id: passwordField
                Layout.fillWidth: true
                echoMode: TextInput.Password;
            }
            Label { text: "changed on" }
            TextField {
                id: changedOnField
                Layout.fillWidth: true
            }
            Label { text: "expiring on" }
            TextField {
                id: expiringOnField
                Layout.fillWidth: true
            }
            Label { text: "description" }
            TextField {
                id: descriptionField
                Layout.fillWidth: true
            }
            Label { text: "notes" }
            TextArea {
                id: notesField
                Layout.fillWidth: true
            }
            Label { text: "login challenge" }
            TextField {
                id: loginChallengeField
                Layout.fillWidth: true
            }
            Label { text: "forgot passwd challenge" }
            TextField {
                id: forgotPasswordChallengeField
                Layout.fillWidth: true
            }
            Label { text: "secret notes" }
            TextArea {
                id: secretNotesField
                Layout.fillWidth: true
            }
            Label { text: "tags" }
            TextField {
                id: tagsField
                Layout.fillWidth: true
            }
            Label { }
            RowLayout {
                Button {
                    id: btnOK
                    text: "OK"
                    onClicked: function() {
                        var j = {
                          login: loginField.text,
                          password: passwordField.text,
                          tags: tagsField.text.split(',')
                        }
                        if (tagsField.text.length == 0) j['tags'] = []
                        if (changedOnField.text.length > 0) j['changedOn'] = changedOnField.text
                        if (expiringOnField.text.length > 0) j['expiringOn'] = expiringOnField.text
                        if (descriptionField.text.length > 0) j['description'] = descriptionField.text
                        if (notesField.text.length > 0) j['notes'] = notesField.text
                        if (loginChallengeField.text.length > 0) j['loginChallenge'] = loginChallengeField.text
                        if (forgotPasswordChallengeField.text.length > 0) j['forgotPasswordChallenge'] = forgotPasswordChallengeField.text
                        if (secretNotesField.text.length > 0) j['secretNotes'] = secretNotesField.text
                        putStr(JSON.stringify(j))
                        mainwin.close()
                    }
                }
                Button {
                    id: btnCancel
                    text: "Cancel"
                    onClicked: function() {
                        putStr("cancel")
                        mainwin.close()
                    }
                }
            }
        }
    }
}
