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
    await driver.get(conf.sites.sbf.url)
    const locLogin = s.By.name("nome")
    const locPass = s.By.name("lenha")
    const locLoginBtn = s.By.name("B1")
    await driver.wait(s.until.elementLocated(locLoginBtn))

    amkhlv.withPassword(
        conf.pwdPath,
        conf.sites.sbf.nick,
        conf.sites.sbf.login,
        async (pwd) => {
            await driver.findElement(locLogin).sendKeys(conf.sites.sbf.login)
            await driver.findElement(locPass).sendKeys(pwd)
            await driver.findElement(locLoginBtn).click()


        }
        )
}

main()
