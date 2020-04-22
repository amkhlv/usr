import amkhlv = require('@amkhlv/utils')

amkhlv.withPassword(
    "/path/to/my/passwords.xml.gpg",
    "NET",
    "andrei",
    x => { console.log(x) }
    )
