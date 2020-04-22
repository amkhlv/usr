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


}


main()
