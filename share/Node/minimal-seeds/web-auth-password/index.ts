import * as passportLocal from 'passport-local'
import express = require('express')
import path = require('path')
import yaml = require('js-yaml')
import fs = require('fs')
import os = require('os')
import passport = require('passport')
import session = require('express-session')
import bodyParser = require('body-parser')
import csrf = require('csurf')
import crypto = require('crypto')
import sql = require('sqlite3')
import sqlStore = require('connect-sqlite3')

const LocalStrategy = passportLocal.Strategy

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

declare global {
    namespace Express {
      interface User { username: string }
    }
}
interface User { login: string; hash: string; salt: string }
interface Config {
  prefix: string;
  sessionSecret: string;
  port: number;
  users: User[];
  workingPath: string;
  sqliteFile: string;
  staticPath: string;
}

const conf = yaml.safeLoad(fs.readFileSync('config.yaml', 'utf8')) as Config

const prefix = conf.prefix

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ===================== config =====================================

const sessionSecret = conf.sessionSecret
const port: number = conf.port
const users: User[] = conf.users
const logins: string[] = users.map(u => u.login)

const db = new sql.Database(path.join(conf.workingPath, conf.sqliteFile))

const SQLiteStore = sqlStore(session)

// ====================== App init ===================================
const app = express()
app.set('views', path.join(__dirname, 'views'))
app.set('view engine', 'pug')

// ====================== Passport.js config =========================
passport.use(new LocalStrategy(
  (username:string, password:string, done) => {
    if (logins.includes(username)) {
      const a = users.find(u => {
        const h = crypto.scryptSync(password, u.salt, 64).toString('hex')
        return (u.login === username) && (h === u.hash)
      })
      if (a === undefined) {
        return done(null, false, { message: 'wrong password' })
      } else {
        return done(null, username)
      }
    } else { return done(null, false, { message: 'no such user' }) }
  }
))
passport.serializeUser((user, cb) => {
  cb(null, user)
})
passport.deserializeUser((id, cb) => {
  if ((typeof id === 'string') && (logins.includes(id))) {
    cb(null, { username: id })
  } else { cb(new Error('ERROR: User Not Found...')) }
})
const parseForm = bodyParser.urlencoded({ extended: false })
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ HERE should be cookie: false ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
const csrfProtection = csrf({ cookie: false })
// const csrfProtection = csrf({ cookie: true }) // <-- WRONG !
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
function checkPWD (ruser:string|undefined) {
  return (typeof ruser === 'string') && (ruser.length > 2) && (logins.includes(ruser))
}

app.use(parseForm)
app.use(session({
  store: SQLiteStore({ dir: conf.workingPath, db: conf.sqliteFile }),
  secret: sessionSecret,
  resave: false,
  saveUninitialized: false
}))
app.use(passport.initialize())
app.use(passport.session())
app.use(csrfProtection)
app.use(express.static(conf.staticPath))
app.use((err, req, res, next) => {
  if (err.code !== 'EBADCSRFTOKEN') return next(err)
  // handle CSRF token errors here
  res.status(403)
  res.send('form tampered with')
})
app.use((req, res, next) => {
  if (req.path === '/login' || (req.user !== undefined && checkPWD(req.user.username))) {
    next()
  } else {
    console.log(`DISALLOWING ${req.user ? req.user.username : 'UNKNOWN'}`)
    res.redirect(prefix + '/login')
  }
})

// ====================== Authentication routes ======================
app.get('/login',
  (req, res) => {
    res.render('login', { csrfToken: req.csrfToken(), prefix: prefix })
  })
app.post('/login',
  parseForm,
  passport.authenticate('local', { failureRedirect: prefix + '/login' }),
  (req, res) => {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log('AUTH OK')
    res.redirect(prefix + '/')
  })
app.get('/logout',
  (req, res) => {
    req.logout()
    res.redirect(prefix + '/')
  })

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

app.get('/',
  (req, res) => {
    res.render(
      'main',
      {
        ttl: `Welcome ${req.user ? req.user.username : 'UNKNOWN'}!`,
        msg: `Hi ${req.user ? req.user.username : 'UNKNOWN'} !`,
        prefix: prefix,
        csrfToken: req.csrfToken()
      })
  })
app.post('/input',
  parseForm,
  (req, res) => {
    res.render(
      'main',
      {
        prefix: prefix,
        csrfToken: req.csrfToken(),
        ttl: `Welcome ${req.user ? req.user.username : 'UNKNOWN'}!`,
        msg: `I heard: ${req.body.msg}`
      })
  }
)
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`)
})
