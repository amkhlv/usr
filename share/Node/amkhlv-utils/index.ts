import proc = require('child_process')
import xml = require('xml2js')

export async function wait(ms: number) {
    return new Promise((resolve, reject) => {
        setTimeout(() => resolve("OK"), ms);
    })
}

export function withPassword(
    pwdFilePath: string,
    website: string,
    login: string,
    cb: (pwd: string) => void
) {
    proc.exec(
        "gpg --decrypt '" + pwdFilePath + "'",
        (error, stdout, stderr) => {
            xml.parseString(
                stdout,
                (e,r) => {
                    const site : {
                        $ : { nick : string },
                        account : { 
                            $ : { login : string },
                            password : { _ : string}[] 
                        }[]
                    }[]  = r['xml']['sites'][0]['site']
                    const accountss = site.filter(
                        s => s.$.nick === website
                        ).map(
                            s => s.account.filter(a => a.$.login === login)
                            )
                    if (accountss.length === 0) {
                        throw new Error("site not found")
                    } else if (accountss[0].length === 0) {
                        throw new Error("login not found")
                    } else { 
                        const x = accountss[0][0]['password'][0]
                        cb(x._ ?? JSON.stringify(x).slice(1,-1))
                    }
                }
            )
        }
    )
}