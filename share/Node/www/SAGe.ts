import s = require('selenium-webdriver');
import ff = require('selenium-webdriver/firefox');
import fs = require('fs');
import os = require('os');
import path = require('path')
import yaml = require('js-yaml')
import amkhlv = require('@amkhlv/utils')


async function main() {
    const conf = yaml.safeLoad(fs.readFileSync(path.join(os.homedir(), ".config/amkhlv/www.yaml"), "utf8"))
    const options = new ff.Options();
    options.setProfile(conf.profileInsecure)
    const builder = new s.Builder().forBrowser('firefox')
    builder.setFirefoxOptions(options)
    const driver = await builder.build()
    await driver.get(conf.sites.sage.url)
    const locateLoginBtn = s.By.className("botao_final")
    const btn = await driver.findElement(locateLoginBtn)
    await driver.wait(s.until.elementLocated(locateLoginBtn))
    amkhlv.withPassword(
        conf.pwdPath,
        conf.sites.sage.nick,
        conf.sites.sage.login,
        async (pwd) => {
            await driver.findElement(s.By.name("login")).sendKeys(conf.sites.sage.login)
            await driver.findElement(s.By.name("password")).sendKeys(pwd)
            await btn.click()
            await driver.wait(s.until.elementLocated(s.By.id("oT_topoT_3")))
            await (await driver.findElement(s.By.id("ptLink"))).click()
            // await ptLink.click()


        }
        )

}


main()
