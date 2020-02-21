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


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

const conf = yaml.safeLoad(
  fs.readFileSync(
    path.join(os.homedir(), ".config/amkhlv/localsite/config.yaml"),
    "utf8"))
const mainPage = '/'
const calPath = path.join(conf.workingPath , conf.calendarFile)
const musPath = path.join(conf.workingPath , conf.musicFile)
const prefix = conf.prefix

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ===================== config =====================================

const sessionSecret = conf.sessionSecret
const port: number = conf.port; // default port to listen
class User { login: string; hash: string; salt: string }
const users: User[] = conf.users
const logins = users.map(u => u.login)

const db = new sql.Database(path.join(conf.workingPath , conf.sqliteFile))

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
  store: SQLiteStore({dir: conf.workingPath , db: conf.sqliteFile}),
  secret: sessionSecret,
  resave: false,
  saveUninitialized: false,
  cookie: {secure : false}
}));
app.use(passport.initialize());
app.use(passport.session());

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ HERE should be cookie: false ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
const csrfProtection = csrf({ cookie: false })
// const csrfProtection = csrf({ cookie: true }) // <-- WRONG !
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ====================== Static files are in folder public/ =========
app.use(csrfProtection)
app.use(express.static(conf.staticPath))
app.use((err, req, res, next) => {
  if (err.code !== 'EBADCSRFTOKEN') return next(err)
  // handle CSRF token errors here
  res.status(403)
  res.send('form tampered with')
})

// ====================== Authentication routes ======================
app.get('/login',
  (req, res) => {
  res.render('login', {csrfToken: req.csrfToken(), 'prefix': prefix});
  });
app.post('/login',
  parseForm,
  passport.authenticate('local', { failureRedirect: prefix + '/login' }),
  (req, res) => {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log("AUTH OK")
    res.redirect(prefix + mainPage);
  });
app.get('/logout',
  (req, res) => {
    req.logout();
    res.redirect(prefix + '/');
  });


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

function checkPWD(ruser) {
  return (typeof ruser === 'string') && (ruser.length > 2) && (logins.includes(ruser))
}
app.get("/",
  (req,res) => {
  if (checkPWD(req.user)) {
    console.log(`-- allowing ${req.user}`)
    res.render("index", {'homedir': path.join(os.homedir()).toString(), 'prefix': prefix});
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed")
    res.redirect(prefix + '/login')
  }
})
app.get("/music",
  (req, res) => {
  if (checkPWD(req.user)) {
    console.log(`-- allowing ${req.user} to music`)
    const myaml = yaml.safeLoad(fs.readFileSync(musPath, 'utf8'))
    res.render("bookmarks", { 'ttl': 'Music', 'myaml': myaml, 'ncols': 3, 'prefix': prefix });
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed to /music")
    res.redirect(prefix + '/login')
  }
});
app.get("/calendar",
  (req, res) => {
  if (checkPWD(req.user)) {
    res.sendFile(calPath);
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed to /calendar")
    res.redirect(prefix + '/login')
  }
})
app.get("/todo",
  (req,res) => {
  if (checkPWD(req.user)) {
    const todos = db.all(
      `select todo td, note nt, importance im from todolist`, (err, rows) => {
        res.render("todolist", {'prefix': prefix, 'csrfToken': req.csrfToken(), 'rows' : rows})
      }
    )
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed to /todo")
    res.redirect(prefix + '/login')
  }
})
app.post("/tododelete",
  parseForm,
  (req,res) => {
  if (checkPWD(req.user)) {
    console.log(JSON.stringify(req.body, null, 2))
    db.run(
      `delete from todolist where todo = ? and note = ? and importance = ?`,
      [req.body.td, req.body.nt, req.body.im],
      err => {}
    )
    res.redirect(prefix + '/todo')
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed to delete a TODO")
    res.redirect(prefix + '/login')
  }
})
app.post("/todoadd", 
  parseForm,
  (req,res) => {
  if (checkPWD(req.user)) {
    console.log(JSON.stringify(req.body, null, 2))
    db.run(
      `insert into todolist values (?,?,?)`,
      [req.body.td, req.body.nt, req.body.im],
      err => {}
    )
    res.redirect(prefix + '/todo')
  } else {
    console.log("USER>>>" + req.user + "<<< not allowed to delete a TODO")
    res.redirect(prefix + '/login')
  }
})
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
