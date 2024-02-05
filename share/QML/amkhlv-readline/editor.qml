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

        property string login: "";
        onLoginChanged: loginField.text = login;
        property string password: "";
        onPasswordChanged: passwordField.text = password;
        property string changedOn: "";
        onChangedOnChanged: changedOnField.text = changedOn;
        property string expiringOn: "";
        onExpiringOnChanged: expiringOnField.text = expiringOn;
        property string description: "";
        onDescriptionChanged: descriptionField.text = description;
        property string notes: "";
        onNotesChanged: notesField.text = notes;
        property string loginChallenge: "";
        onLoginChallengeChanged: loginChallengeField.text = loginChallenge;
        property string forgotPasswordChallenge: "";
        onForgotPasswordChallengeChanged: forgotPasswordChallengeField.text = forgotPasswordChallenge;
        property string secretNotes: "";
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
        property bool isConfirmation: false;
        onIsConfirmationChanged: function () {
            mainwin.title = "Delete account ?"
            actionLabel.text = "Delete account?"
            if (isConfirmation) {
                var normalFields = [ loginField, passwordField, changedOnField, expiringOnField, descriptionField, notesField, loginChallengeField, tagsField ]
                for (var i=0; i < normalFields.length; i++) { normalFields[i].readOnly = true }
                btnOKText.text = "yes"
                btnCancelText.text = "no"
            }
        }
        property string bgOKButton: "white";
        onBgOKButtonChanged: function() {
            btnOKBg.color = bgOKButton
        }
        property string fgOKButton: "black";
        onFgOKButtonChanged: function() {
            btnOKText.color = fgOKButton
        }
        property string bgCancelButton: "white";
        onBgCancelButtonChanged: function() {
            btnCancelBg.color = bgCancelButton
        }
        property string fgCancelButton: "black";
        onFgCancelButtonChanged: function() {
            btnCancelText.color = fgCancelButton
        }
        property string bgEditorWindow: "";
        onBgEditorWindowChanged: mainwin.color = bgEditorWindow;
        property string bgNormalField: "";
        onBgNormalFieldChanged: function () {
            var normalFieldBackgrounds = [ loginFieldBg, passwordFieldBg, changedOnFieldBg, expiringOnFieldBg, descriptionFieldBg, notesFieldBg, loginChallengeFieldBg, tagsFieldBg ]
            for (var i = 0; i < normalFieldBackgrounds.length; i++) {
                normalFieldBackgrounds[i].color = bgNormalField
            }
        }
        property string fgNormalField: "";
        onFgNormalFieldChanged: function () {
            var normalFields = [ loginField, passwordField, changedOnField, expiringOnField, descriptionField, notesField, loginChallengeField, tagsField ]
            for (var i = 0; i < normalFields.length; i++) {
                normalFields[i].color = fgNormalField
            }
        }
        property string bgSecretField: "";
        onBgSecretFieldChanged: function () {
            var secretFieldBackgrounds = [ forgotPasswordChallengeFieldBg, secretNotesFieldBg ]
            for (var i = 0; i < secretFieldBackgrounds.length; i++) {
                secretFieldBackgrounds[i].color = bgSecretField
            }
        }
        property string fgSecretField: "";
        onFgSecretFieldChanged: function () {
            var secretFields = [ forgotPasswordChallengeField, secretNotesField ]
            for (var i = 0; i < secretFields.length; i++) {
                secretFields[i].color = fgSecretField
            }
        }
    }

    GroupBox {
        id: box
        GridLayout {
            columns: 2;
            Label{ text: "login"; font.pointSize: 14 }
            TextField {
                id: loginField
                font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: loginFieldBg }
            }
            Label { text: "password"; font.pointSize: 14  }
            TextField {
                id: passwordField
        font.pointSize: 14
                Layout.fillWidth: true
                echoMode: TextInput.Password;
                background: Rectangle { id: passwordFieldBg }
            }
            Label { text: "changed on"; font.pointSize: 14 }
            TextField {
                id: changedOnField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: changedOnFieldBg }
            }
            Label { text: "expiring on"; font.pointSize: 14 }
            TextField {
                id: expiringOnField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: expiringOnFieldBg }
            }
            Label { text: "description"; font.pointSize: 14 }
            TextField {
                id: descriptionField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: descriptionFieldBg }
            }
            Label { text: "notes"; font.pointSize: 14 }
            TextArea {
                id: notesField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: notesFieldBg }
            }
            Label { text: "login challenge"; font.pointSize: 14 }
            TextField {
                id: loginChallengeField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: loginChallengeFieldBg }
            }
            Label { text: "forgot passwd challenge"; font.pointSize: 14 }
            TextField {
                id: forgotPasswordChallengeField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: forgotPasswordChallengeFieldBg}
            }
            Label { text: "secret notes"; font.pointSize: 14 }
            TextArea {
                id: secretNotesField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: secretNotesFieldBg }
            }
            Label { text: "tags"; font.pointSize: 14 }
            TextField {
                id: tagsField
        font.pointSize: 14
                Layout.fillWidth: true
                background: Rectangle { id: tagsFieldBg }
            }
            Label { id: actionLabel; text: "save?"; font.pointSize: 14 }
            RowLayout {
                Button {
                    id: btnOK
                    background: Rectangle {
                        id: btnOKBg
                        implicitWidth: btnOKText.implicitWidth
                        implicitHeight: btnOKText.implicitHeight
                        Text { id: btnOKText ; text: "OK"; color: "black"; font.pointSize: 14 }
                    }
                    onClicked : function () {
                        if (st.isConfirmation) {
                            putStr("yes");
                            mainwin.close();
                        } else {
                            var j = {
                                login: loginField.text,
                                password: passwordField.text,
                                tags: tagsField.text.split(',')
                            }
                            if (tagsField.text.length === 0) j['tags'] = []
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
                }
                Button {
                    id: btnCancel
                    background: Rectangle {
                        id: btnCancelBg
                        implicitWidth: btnCancelText.implicitWidth
                        implicitHeight: btnCancelText.implicitHeight
                        Text { id: btnCancelText; text:"Cancel"; color: "black"; font.pointSize: 14 }
                    }
                    onClicked : function () {
                        if (st.isConfirmation) {
                            putStr("no");
                            mainwin.close();
                        } else {
                            putStr("cancel");
                            mainwin.close();
                        }
                    }
                }
            }
        }
    }
}
