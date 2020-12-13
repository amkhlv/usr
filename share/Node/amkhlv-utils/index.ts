import proc = require('child_process')
import xml = require('xml2js')
import fs = require('fs')
import os = require('os')
import path = require('path')
import querystring = require('querystring');
const axios = require('axios')

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

export function getBibTeX(k: string) : Promise<Map<string,string>> {
  const xmlfile = path.join(os.homedir(), ".config", "amkhlv", "latex2svg.xml")
  console.log(`About to get BibTeX for: >>>${k}<<<`);
  return new Promise(
    (resolve, reject) => {
      fs.readFile(
        xmlfile,
        (err, xmlconf) => {
          if (err === null) {
            (new xml.Parser()).parseString(
              xmlconf,
              (e:Error, r:any)=> { 
                const server_params: {server: {host: string, port: string, bibpath: string, token: string}} = r
                if (e === null) { 
                  axios({
                    method: 'POST',
                    url: `http://${server_params.server.host}:${server_params.server.port}/${server_params.server.bibpath}?${querystring.stringify({'k': k, 'token': server_params.server.token})}`,
                    headers: {'BystroTeX': 'yes'}
                  }
                  ).then((response:any)=> {
                    (new xml.Parser()).parseString(
                      response.data, 
                      (xmlerr:Error, o:{bibentry: {v: Array<{ $ : {key: string}, '_' : string}>}})=> {
                        if (xmlerr === null) { 
                          const bibitem = new Map()
                          for (let x of o['bibentry']['v']) {
                            bibitem.set(x['$']['key'], x['_'])
                          }
                          resolve(bibitem) 
                        } else { 
                          reject(xmlerr) 
                        }
                      }
                    )
                  }).catch((axios_error:Error)=> reject(axios_error))
                } else { 
                  reject(e) 
                }
              }
            )
          } else { 
            reject(err) 
          }
        }
      )
    }
  )
}

      



