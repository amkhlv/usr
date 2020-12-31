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
import formidable = require('formidable')

const LocalStrategy = passportLocal.Strategy

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

declare global {
    namespace Express {
      interface User { username: string }
    }
}
interface User {
  login: string;
  hash: string;
  salt: string;
}
interface Config {
  prefix: string;
  sessionSecret: string;
  port: number;
  users: User[];
  workingPath: string;
  storagePath: string;
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
function move(oldPath, newPath, callback) {
  fs.rename(oldPath, newPath, function (err) {
    if (err) {
      if (err.code === 'EXDEV') {
        copy()
      } else {
        callback(err)
      }
      return
    }
    callback()
  })
  function copy () {
    const readStream = fs.createReadStream(oldPath)
    const writeStream = fs.createWriteStream(newPath)

    readStream.on('error', callback)
    writeStream.on('error', callback)

    readStream.on('close', function () {
      fs.unlink(oldPath, callback)
    })

    readStream.pipe(writeStream)
  }
}
app.get('/',
  (req, res) => {
    if (req.user && 'username' in req.user) {
      const owner = req.user.username
      const query = `SELECT 
      sha256 sha256,
      filename filename,
      dt dt,
      comment comment
      FROM files
      WHERE owner = ?`
      db.all(query, [req.user.username], (err, rows) => {
        if (err) {
          console.log(err)
          res.status(500).render('error')
        } else {
          res.render(
            'main',
            {
              ttl: owner,
              page_title: `${owner}'s files`,
              msg: `Hi ${owner} !`,
              prefix: prefix,
              csrfToken: req.csrfToken(),
              files: rows
            }
          )
        }
      })
    } else { res.status(403).render('not-allowed') }
  }
)
app.post('/upload',
  (req, res) => {
    const form = new formidable.IncomingForm()
    form.parse(req, function (err, fields, files) {
      if (err) {
        console.log(err)
        res.status(500).render('error')
      } else {
        let oldpath:string
        if ('path' in files.filetoupload) {
          oldpath = files.filetoupload.path
          console.log(oldpath)
          let nm: string
          if ('name' in files.filetoupload) { nm = files.filetoupload.name }
          const owner = req.user ? req.user.username : 'IMPOSTOR'
          const hash = crypto.createHash('sha256')
          const s = fs.createReadStream(oldpath)
          s.on('data', data => { hash.update(data) })
          s.on('end', () => {
            const sha256 = hash.digest('hex')
            const sql = 'INSERT INTO files VALUES (?,?,?,datetime(),?)'
            db.run(sql, [sha256, owner, nm, fields.comment], error => {
              if (error) {
                console.log(error)
                res.status(500).render('error')
              } else {
                const newpath = path.join(conf.storagePath, owner, sha256)
                move(oldpath, newpath, (err: Error|undefined) => {
                  if (err) {
                    console.log(err)
                    res.status(500).render('error')
                  } else {
                    res.redirect(prefix + '/')
                  }
                })
              }
            })
          })
        } else {
          console.log('MULTIPLE FILES?')
        }
      }
    })
  }
)

app.post('/delete', (req, res) => {
  if (req.user && 'username' in req.user) {
    const owner = req.user.username
    const form = new formidable.IncomingForm()
    form.parse(req, function (err, fields, files) {
      if (err) {
        console.log(err)
        res.status(500).render('error')
      } else {
        const filepath = path.join(conf.storagePath, owner, fields.sha as string)
        const filename = fields.filename as string
        const sql = `DELETE FROM files
          WHERE owner = ? AND sha256 = ? AND filename = ?`
        db.run(sql, [owner, fields.sha as string, filename], error => {
          if (error) {
            console.log(error)
            res.status(500).render('error')
          } else {
            fs.unlink(filepath, (err) => {
              if (err) {
                console.log(err)
                res.status(500).render('error')
              } else {
                res.redirect(prefix + '/')
              }
            })
          }
        })
      }
    })
  } else { res.status(403).render('not-allowed') }
})

app.get('/download', (req, res) => {
  if (req.user && 'username' in req.user) {
    const owner = req.user.username
    const sha = req.query.sha as string
    const sql = `SELECT
    filename filename
    FROM files
    WHERE owner = ? and sha256 = ?`
    db.all(sql, [owner, sha], (err, rows) => {
      if (err) {
        console.log(err)
        res.status(500).render('error')
      } else {
        rows.forEach((row, _) => {
          res.download(path.join(conf.storagePath, owner, sha), row.filename)
        })
      }
    })
  } else { res.status(403).render('not-allowed') }
})

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`)
})
