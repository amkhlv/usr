Mutt
====

## Starting

    mutt -f /path/to/mymaildir

## Navigation

### Withing mail index

To go to the last message press `*`

### To switch to different mailbox

Press `c`

## Tagging messages

To __tag__ an individual message, press `t`.

To __tag__ multiple messages, press `T` then enter pattern then `ENTER`

To __untag__ messages, press `Ctrl-t` then pattern then `ENTER`

The patterns are [listed here](http://www.mutt.org/doc/manual/manual.html#patterns). 
In particular:

1. `~N` for new messages

1. `~d [MIN]-[MAX]` for messages with “date-sent” in a Date range

1. `~D` for deleted messages

1. `~e EXPR` for messages which contains EXPR in the "Sender" field

## Operations on many tagged messages

To set flag:

    ;w...

To clear flag:

    ;W...

For example, to remove the `N` (new, unread) flag:

    ;WN

The semicolon `;` is called "tag-prefix". 
For those commands which have keyboard shortcut `X`, pressing `;X` applies that command to all tagged messages.
For example:

    ;d

--- __deletes__ all tagged messages (then, to fully expunge them, have to press `$1` to __sync__)

For those commands which do not have keyboard shortcut, execute:

    :exec tag-prefix COMMAND


## Managing flags

    w,W      add, remove flags

## Read, unread, new...

there are 3 states for an email in mutt:

    New - message is new and unread (indicated by a N in the first column of the index)

    Old - message is old and unread (indicated by an O in the first column of the index)

    Read - message has been presented to the user (nothing in the first column of the index)

## Viewing message, saving attachments, etc

By default, `mutt` saves attachments to the folder where it was started.

