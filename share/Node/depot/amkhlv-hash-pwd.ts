import crypto = require('crypto')
import readline = require('readline');

const salt = process.argv[2]

console.log("USING SALT:>>>" + salt + "<<<")


readline.emitKeypressEvents(process.stdin);
process.stdin.setRawMode(true);

console.log("Enter password and press ENTER:")

let pwd = ""
process.stdin.on('keypress', (str, key) => {
    if (key.hasOwnProperty('name') && key.name !== 'return') {
        pwd = pwd + str
    } else {
        crypto.scrypt(pwd, salt, 64, (err, derivedKey) => {
            if (err) { throw err }
            console.log(derivedKey.toString('hex')); 
            process.exit(0)
        });
        // console.log(pwd)
    }
})



