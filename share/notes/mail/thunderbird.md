# Installing

    apt-get install -t experimental icedove
    apt-get install -t experimental iceowl-extension

(The `iceowl` is `Lightning`)

# `SOGo` for `VCard`

<div>
    <span style="color:red;"><b> Birthdays will not sync if YEAR is not present; put 1900</b></span>
</div>

I have to install the `SOGo` connector extension. I got it from [GitHub](https://github.com/inverse-inc/sogo-connector.tb24)

Sometimes it may happen that the synchronization fails. In this case, I have to first \`\`Abort Synchronization'' and then
start sync again. (But if I do not \`\`Abort sync'' then hanging all sync will prevent new sync from happening.)
Process of syncing should be indicated by the status bar.

# Using

__Compact Folders__ under `File` menu seems to be an important thing to do from time to time. I have not yet figured out
exactly when.

## Configuring account with local `Dovecot`

After account configuration, right-click on account inbox, then click on `Subscribe`. The list of folders advertised by the
`Dovecot` should appear. Check those you want. 

## Deleting messages

In Server Settings, choose the deleted messages to be \`\`marked as deleted''.
Then, after they are marked for deletion, choose File → Compact Folders, this will get them Expunged

## Disable anti-spam

In `Junk Settings` disable \`\`adaptive junk mail controls''

## Configuring `sendmail` for the outgoing mail

In the `Outgoing Server (SMTP)` enter:

    Description: sendmail
    Server Name: localhost
    Port:        25
    Authentication Method: No authentication
    Connection Security: None

# Command line tricks

To open an email file, first of all:

1. Make sure that the file has extension `.eml`

The command is:

    icedove -file filename.eml &



