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
import crypto = require('crypto')

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

const conf = yaml.safeLoad(
  fs.readFileSync(
    path.join(os.homedir(), ".config/amkhlv/localsite/config.yaml"),
    "utf8"))

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ===================== config =====================================

const sessionSecret = conf.sessionSecret
const port: number = conf.port; // default port to listen
class User { login: string; hash: string; salt: string }
const users: User[] = conf.users
const logins = users.map(u => u.login)

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
app.use(bodyParser.urlencoded({ extended: false }))
app.use(session({ secret: sessionSecret, resave: false, saveUninitialized: false }));
app.use(passport.initialize());
app.use(passport.session());

// ====================== Static files are in folder public/ =========
app.use(express.static('public'))

// ====================== Authentication routes ======================
app.get('/login',
  (req, res) => {
    res.render('login');
  });
app.post('/login',
  passport.authenticate('local', { failureRedirect: '/login' }),
  (req, res) => {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log("AUTH OK")
    res.redirect('/');
  });
app.get('/logout',
  (req, res) => {
    req.logout();
    res.redirect('/');
  });


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

app.get("/", (req, res) => {
  if ((typeof req.user === 'string') && (logins.includes(req.user))) {
    console.log(`-- allowing ${req.user} to enter`)
    res.render("main", { 'ttl': `Welcome ${req.user}!` });
  } else {
    console.log("USER>>>" + req.user + "<<< is not allowed")
    res.redirect('/login')
  }
});
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
