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

    driver.get(conf.sites.celpebras.url)

    amkhlv.withPassword(
        conf.pwdPath,
        conf.sites.celpebras.nick,
        conf.sites.celpebras.login,
        async (pwd) => {
            const btnLocator = s.By.id("loginSso")
            await driver.wait(s.until.elementLocated(btnLocator))
            await driver.wait(s.until.elementIsVisible(driver.findElement(btnLocator)))
            await amkhlv.wait(9000)
            await (await driver.findElement(btnLocator)).click()
            const loginLocator = s.By.id("username")
            await driver.findElement(loginLocator).sendKeys(conf.sites.celpebras.login)
            await driver.findElement(s.By.id("password")).sendKeys(pwd)
            await amkhlv.wait(5000)
            const kcLocator = s.By.id("kc-login")
            await driver.wait(s.until.elementLocated(kcLocator))
            await (await driver.findElement(kcLocator)).click()


        }
        )


}


main()
