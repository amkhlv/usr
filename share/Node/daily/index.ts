import express = require("express")
import path = require("path")
import yaml = require('js-yaml')
import fs = require('fs')
import os = require('os')
import passport = require('passport')
import * as passportLocal from 'passport-local';
const LocalStrategy = passportLocal.Strategy;
import session = require('express-session')
import bodyParser = require('body-parser')
import csrf = require('csurf')
import crypto = require('crypto')
import sql = require('sqlite3')
import sqlStore = require('connect-sqlite3')
import connect = require('connect')
import shell = require('child_process')


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

const conf = yaml.safeLoad(
  fs.readFileSync(path.join(os.homedir(), ".config/amkhlv/daily/config.yaml"), "utf8")
)
const prefix = conf.prefix
const bin = path.join(os.homedir(), ".config/amkhlv/daily/bin/")

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ===================== config =====================================

const sessionSecret = conf.sessionSecret
const port: number = conf.port; // default port to listen
class User { login: string; hash: string; salt: string }
class RequiredHeader { header: string; value: string }
class ToDo {description: string; script: string}
const users: User[] = conf.users
const logins = users.map(u => u.login)
const requiredHeaders: RequiredHeader[] = conf.checkHeaders
function todos(): ToDo[] { return yaml.safeLoad(
  fs.readFileSync(path.join(os.homedir(), ".config/amkhlv/daily/list.yaml"), "utf8")
)}
const dumpFile = conf.dumpFile

const db = new sql.Database(path.join(conf.workingPath, conf.sqliteFile))

const SQLiteStore = sqlStore(session)

// ====================== App init ===================================
const app = express();
app.set("views", path.join(__dirname, "views"));
app.set("view engine", "ejs");

// ====================== Passport.js config =========================
passport.use(new LocalStrategy(
  (username, password, done) => {
    if (logins.includes(username)) {
      const a = users.find(u => {
        const h = crypto.scryptSync(password, u.salt, 64).toString('hex')
        return (u.login === username) && (h === u.hash)
      })
      if (a === undefined) {
        return done(null, false, { message: "wrong password" })
      } else {
        return done(null, username)
      }
    } else { return done(null, false, { message: "no such user" }) }
  }
))
passport.serializeUser((user, cb) => {
  cb(null, user);
});
passport.deserializeUser((id, cb) => {
  if ((typeof id === 'string') && (logins.includes(id))) {
    cb(null, id)
  } else { cb('ERROR: User Not Found...') }
});
const parseForm = bodyParser.urlencoded({ extended: false })
app.use(parseForm)
app.use(session({
  store: SQLiteStore({ dir: conf.workingPath, db: conf.sqliteFile }),
  secret: sessionSecret,
  resave: false,
  saveUninitialized: false
}));
app.use(passport.initialize());
app.use(passport.session());

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ HERE should be cookie: false ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
const csrfProtection = csrf({ cookie: false })
// const csrfProtection = csrf({ cookie: true }) // <-- WRONG !
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

function checkPWD(ruser) {
  return (typeof ruser === 'string') && (ruser.length > 2) && (logins.includes(ruser))
}
function checkHeaders(req: express.Request): boolean {
  return requiredHeaders.map(rh => (req.header(rh.header) === rh.value)).reduce((acc, x) => (acc && x), true)
}
app.use(csrfProtection)
app.use(express.static(conf.staticPath))
app.use((err, req, res, next) => {
  if (err.code !== 'EBADCSRFTOKEN') return next(err)
  // handle CSRF token errors here
  res.status(403)
  res.send('form tampered with')
})
app.use((req, res, next) => {
  if (req.path === "/login" || checkPWD(req.user)) {
    next()
  } else {
    console.log(`DISALLOWING ${req.user}`)
    res.redirect(prefix + "/login")
  }
})
app.use((req, res, next) => {
  if (checkHeaders(req)) {
    console.log(`headers OK`)
    next()
  } else {
    res.status(403).send(`missing required headers ${requiredHeaders}`)
  }
})

// ====================== Authentication routes ======================
app.get('/login',
  (req, res) => {
    res.render('login', { csrfToken: req.csrfToken(), 'prefix': prefix });
  });
app.post('/login',
  parseForm,
  passport.authenticate('local', { failureRedirect: prefix + '/login' }),
  (req, res) => {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log("AUTH OK")
    res.redirect(prefix + '/');
  });
app.get('/logout',
  (req, res) => {
    req.logout();
    res.redirect(prefix + '/');
  });


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

const tds = todos()
const globalUnchecked = [...tds]
function dumpUnchecked(n: number)  {
  delete globalUnchecked[n]
  fs.writeFileSync(
    dumpFile,
    globalUnchecked.filter((v,_) => v).map((v,_) => v.description)
    )
}
dumpUnchecked(-1)

function pageMap(req: express.Request): { prefix: string, csrfToken: string, ttl: string, todos: ToDo[] } {
  return {
    'prefix': prefix,
    'csrfToken': req.csrfToken(),
    'ttl': `checklist`,
    'todos': todos()
  }
}
app.get("/", (req, res) => { res.render("main", pageMap(req)) })
app.post("/activity",
  parseForm,
  (req, res) => {
    const n: number = +req.body.ord
    if (n < tds.length) {
      dumpUnchecked(n)
      const x = path.join(bin, tds[n].script)
      console.log(`running script: ${x}`)
      shell.exec(x)
      res.redirect(prefix + "/")
    } else {
      console.log("parameter out of range")
      res.redirect(prefix + "/")
    }
  }
)
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
