# Use of `dpkg-reconfigure exim4-config`

    dpkg-reconfigure exim4-config

Sample dpkg-reconfigure answers for `belavista.ift.unesp.br`

- Tipo geral de configuração de e-mail:

        mensagens enviadas por "smarthost"; recebidas por SMTP ou fetchmail

- Nome do sistema de mensagens:

        belavista.ift.unesp.br

- Lista de endereços IP nos quais escutar por conexões SMTP: 

        127.0.0.1 ; ::1

- Outros destinos para os quais mensagens devem ser aceitas:

        LEAVE_BLANK

- Máquinas para as quais fazer "relay":

        LEAVE_BLANK

- Endereço IP ou nome de máquina do "smarthost" de saída:

        smtp.gmail.com::587

- Ocultar nome do domínio local nas mensagens enviadas? 

        Sim

- Nome de domínio visível para usuários locais:

        gmail.com 

- Manter o número de pesquisas DNS mínimas (Discagem-sob-Demanda)?

        Não

- Método de entrega para mensagens locais:

        Formato "mbox" em /var/mail/

- Dividir a configuração em pequenos arquivos? 

        Não

# Password file `/etc/exim4/passwd.client`

have to edit `/etc/exim4/passwd.client` and include the following line in it:

- for gmail:

        *.google.com:yourAccountName@gmail.com:y0uRpaSsw0RD

Also make sure that the permissions on  `/etc/exim4/passwd.client` are: `root Debian-exim`:

    -rw-r-----. 1 root Debian-exim  241 Nov  2 18:09 passwd.client

# Various configuration files

## Check `/etc/mailname`

The content of that file should be:

    belavista.ift.unesp.br

## Check `/etc/hosts`

The first 2 lines should be:

    127.0.0.1       localhost
    127.0.1.1       belavista.ift.unesp.br belavista

(This is very important for the local mail to be delivered correctly)

## Check `/etc/aliases`

This should be OK automatically, but to double check that it is this:

    # /etc/aliases
    mailer-daemon: postmaster
    postmaster: root
    nobody: root
    hostmaster: root
    usenet: root
    news: root
    webmaster: root
    www: root
    ftp: root
    abuse: root
    noc: root
    security: root
    root: andrei


# Cheatsheet from [http://bradthemad.org/tech/notes/exim\_cheatsheet.php](http://bradthemad.org/tech/notes/exim_cheatsheet.php)

## CLI

Print a count of the messages in the queue:

    exim -bpc

Print a listing of the messages in the queue (time queued, size, message-id, sender, recipient):

    exim -bp

Print a summary of messages in the queue (count, volume, oldest, newest, domain, and totals):

    exim -bp | exiqsumm

Print what Exim is doing right now:

    exiwhat

Test how exim will route a given address:

    exim -bt alias@localdomain.com 

Run a pretend SMTP transaction from the command line, as if it were coming from the given IP address. This will display Exim's checks, ACLs, and filters as they are applied. The message will NOT actually be delivered.

    exim -bh 192.168.11.22

Display all of Exim's configuration settings:

    exim -bP

## Searching the queue with exiqgrep

Use -f to search the queue for messages from a specific sender:

    exiqgrep -f [luser]@domain

Use -r to search the queue for messages for a specific recipient/domain:

    exiqgrep -r [luser]@domain

Use -o to print messages older than the specified number of seconds. For example, messages older than 1 day:

    exiqgrep -o 86400 [...]

Use -y to print messages that are younger than the specified number of seconds. For example, messages less than an hour old:

    exiqgrep -y 3600 [...]

Use -s to match the size of a message with a regex. For example, 700-799 bytes:

    exiqgrep -s '^7..$' [...]

Use -z to match only frozen messages, or -x to match only unfrozen messages.

There are also a few flags that control the display of the output.

Use -i to print just the message-id as a result of one of the above two searches:

    exiqgrep -i [ -r | -f ] ...

Use -c to print a count of messages matching one of the above searches:

    exiqgrep -c ...

Print just the message-id of the entire queue:

    exiqgrep -i

## Managing the queue

The main exim binary (`/usr/sbin/exim`) is used with various flags to make things happen to messages in the queue. Most of these require one or more message-IDs to be specified in the command line, which is where `exiqgrep -i` as described above really comes in handy.

Start a queue run:

    exim -q -v

Start a queue run for just local deliveries:

    exim -ql -v

Remove a message from the queue:

    exim -Mrm <message-id> [ <message-id> ... ]

Freeze a message:

    exim -Mf <message-id> [ <message-id> ... ]

Thaw a message:

    exim -Mt <message-id> [ <message-id> ... ]

Deliver a message, whether it's frozen or not, whether the retry time has been reached or not:

    exim -M <message-id> [ <message-id> ... ]

Deliver a message, but only if the retry time has been reached:

    exim -Mc <message-id> [ <message-id> ... ]

Force a message to fail and bounce as "cancelled by administrator":

    exim -Mg <message-id> [ <message-id> ... ]

Remove all frozen messages:

    exiqgrep -z -i | xargs exim -Mrm

Remove all messages older than five days (86400 \* 5 = 432000 seconds):

    exiqgrep -o 432000 -i | xargs exim -Mrm

Freeze all queued mail from a given sender:

    exiqgrep -i -f luser@example.tld | xargs exim -Mf

View a message's headers:

    exim -Mvh <message-id>

View a message's body:

    exim -Mvb <message-id>

View a message's logs:

    exim -Mvl <message-id>

Add a recipient to a message:

    exim -Mar <message-id> <address> [ <address> ... ]

Edit the sender of a message:

    exim -Mes <message-id> <address>

# New mail notification in console

If we want to check for new system messages ON LOGIN (eg on virtual terminal) then the file `/etc/pam.d/login` should contain:

    session    optional   pam_mail.so standard noenv dir=/var/mail

This is probably not that important for me
