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
import vCardJS = require('vcards-js')
import querystring = require('querystring')

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

const conf = yaml.safeLoad(
  fs.readFileSync(
    path.join(os.homedir(), ".config/amkhlv/localsite/config.yaml"),
    "utf8"))
const mainPage = '/'
const calPath = path.join(conf.workingPath, conf.calendarFile)
const musPath = path.join(conf.workingPath, conf.musicFile)
const bmPath = path.join(conf.workingPath, conf.bookmarksFile)
const dailyPath = path.join(conf.workingPath, conf.dailyFile)
const dailyUrgentPath = path.join(conf.workingPath, conf.dailyUrgentFile)
const prefix = conf.prefix

// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ===================== config =====================================

const sessionSecret = conf.sessionSecret
const port: number = conf.port; // default port to listen
class User { login: string; hash: string; salt: string }
const users: User[] = conf.users
const logins = users.map(u => u.login)

const db = new sql.Database(path.join(conf.workingPath, conf.sqliteFile))

const SQLiteStore = sqlStore(session)

// ====================== Global variables ===========================

//const interval = 15000;
const interval = 1000 * 3600 * 23;
const timestamps: Map<string, number> = new Map();

function getTimeStamps() {
  const o:{[key:string]:number} = {};
  timestamps.forEach((v,key) => {
    o[key]= v
  })
  return o
}


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
function checkPWD(ruser) {
  return (typeof ruser === 'string') && (ruser.length > 2) && (logins.includes(ruser))
}
const parseForm = bodyParser.urlencoded({ extended: false })

app.use(parseForm)
app.use(session({
  store: SQLiteStore({ dir: conf.workingPath, db: conf.sqliteFile }),
  secret: sessionSecret,
  resave: false,
  saveUninitialized: false,
  cookie: { secure: false }
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
app.use((req, res, next) => {
  if (req.path === "/login" || checkPWD(req.user)) {
    next()
  } else {
    console.log(`DISALLOWING ${req.user}`)
    res.redirect(prefix + "/login")
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
    res.redirect(prefix + mainPage);
  });
app.get('/logout',
  (req, res) => {
    req.logout();
    res.redirect(prefix + '/');
  });


// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

app.get("/",
  (req, res) => {
    res.render("index", { 'homedir': path.join(os.homedir()).toString(), 'prefix': prefix });
  })
app.get("/music",
  (req, res) => {
    const myaml = yaml.safeLoad(fs.readFileSync(musPath, 'utf8'))
    res.render("bookmarks", { 'ttl': 'Music', 'myaml': myaml, 'ncols': 3, 'prefix': prefix });
  });
app.get("/calendar",
  (req, res) => {
    res.sendFile(calPath);
  })
app.get("/todo",
  (req, res) => {
    const myaml = yaml.safeLoad(fs.readFileSync(dailyPath, 'utf8'))
    const urgent = yaml.safeLoad(fs.readFileSync(dailyUrgentPath, 'utf8'))
    const now = new Date()
    const hoursNow  = now.getTime()
    const stamps = getTimeStamps()
    let ok = true
    urgent.forEach( (item) => {
      if (Array.from(timestamps.keys()).indexOf(item['title']) == -1) { 
        console.log(`Not OK because ${item['title']} missing`) 
        ok = false 
      }
    })
    timestamps.forEach((v,key) => {
      if (hoursNow > v + interval) { 
        console.log(`Not OK because ${v} is overdue`);
        ok = false 
      }
    })
    const todos = db.all(
      `select todo td, note nt, importance im from todolist`, (err, rows) => {
        res.render("todolist", { 
          'prefix': prefix, 
          'csrfToken': req.csrfToken(), 
          'rows': rows , 
          'myaml': myaml, 
          'urgent': urgent, 
          'timestamps': stamps,
          'hoursNow': hoursNow,
          'interval': interval,
          'ok': ok
        })
      }
    )
  })
app.post("/timestamps",
         parseForm,
         (req,res) => {
           const now = new Date();
           timestamps.set(req.body.item, now.getTime());
           res.redirect(req.body.url)
         }
        )
app.post("/tododelete",
  parseForm,
  (req, res) => {
    console.log(JSON.stringify(req.body, null, 2))
    db.run(
      `delete from todolist where todo = ? and note = ? and importance = ?`,
      [req.body.td, req.body.nt, req.body.im],
      err => { }
    )
    res.redirect(prefix + '/todo')
  })
app.post("/todoadd",
  parseForm,
  (req, res) => {
    console.log(JSON.stringify(req.body, null, 2))
    db.run(
      `insert into todolist values (?,?,?)`,
      [req.body.td, req.body.nt, req.body.im],
      err => { }
    )
    res.redirect(prefix + '/todo')
  })
const sqlitecols = [
  'last',
  'first',
  'datecollected',
  'email',
  'workphone',
  'homephone',
  'fax',
  'cellphone',
  'homeaddress',
  'workaddress',
  'website',
  'tags',
  'birthday',
  'notes'
]
app.get("/abk",
  (req, res) => {
    res.render("address", { 'prefix': prefix, 'csrfToken': req.csrfToken() })
  })
app.get("/addressadd",
  (req, res) => {
    res.render(
      "addressade",
      { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'role': "addressadd", 'prefill': {} }
    )
  })
app.post("/addressadd",
  parseForm,
  (req, res) => {
    const s = `INSERT INTO address (` + sqlitecols.join(",") + `) VALUES (` + sqlitecols.map(_ => "?").join(",") + ")"
    const dt = new Date()
    db.run(
      s,
      [
        req.body.ln,
        req.body.fn,
        dt.toISOString().substr(0, 10),
        req.body.eml,
        req.body.wp,
        req.body.hp,
        req.body.fax,
        req.body.cp,
        req.body.ha,
        req.body.wa,
        req.body.web,
        req.body.tags,
        req.body.birthday,
        req.body.notes
      ]
    )
    res.redirect(prefix + '/abk')
  })
app.get("/addressedt",
  (req, res) => {
    res.render(
      "addressade",
      { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'role': "addressedt", 'prefill': req.query })
  })
app.post("/addressedt",
  parseForm,
  (req, res) => {
    const newvals = [
      req.body.ln,
      req.body.fn,
      req.body.dc,
      req.body.eml,
      req.body.wp,
      req.body.hp,
      req.body.fax,
      req.body.cp,
      req.body.ha,
      req.body.wa,
      req.body.web,
      req.body.tags,
      req.body.bd,
      req.body.notes
    ].map(x => x ?? '')
    const oldvals = [
      req.body.oln,
      req.body.ofn,
      req.body.odc,
      req.body.oeml,
      req.body.owp,
      req.body.ohp,
      req.body.ofax,
      req.body.ocp,
      req.body.oha,
      req.body.owa,
      req.body.oweb,
      req.body.otags,
      req.body.obd,
      req.body.onotes
    ].map(x => x ?? '')
    const s = `
    UPDATE address SET
    ` + sqlitecols.map((v, _) => `${v} = ?`).join(" , ") + `
      WHERE
      ` + sqlitecols.map((v, _) => `coalesce(${v},'') = ?`).join(" and ")
    db.run(s, [...newvals, ...oldvals], err => { console.log(err) })
    res.redirect(prefix + '/abk')
  })
app.get("/addressdel",
  (req, res) => {
    res.render(
      "addressade",
      { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'role': "addressdel", 'prefill': req.query })
  })
app.post("/addressdel",
  parseForm,
  (req, res) => {
    const s = `
    DELETE FROM address WHERE
    ` + sqlitecols.map(x => `coalesce(${x},'') = ?`).join(" and ")
    db.run(
      s,
      [
        req.body.ln,
        req.body.fn,
        req.body.dc,
        req.body.eml,
        req.body.wp,
        req.body.hp,
        req.body.fax,
        req.body.cp,
        req.body.ha,
        req.body.wa,
        req.body.web,
        req.body.tags,
        req.body.bd,
        req.body.notes
      ],
      err => { console.log(err) }
    )
    res.redirect(prefix + '/abk')
  }
)
app.post("/addressshow",
  parseForm,
  (req, res) => {
    const q = `SELECT id id, last ln, first fn, datecollected dc, email eml,
    workphone wp, homephone hp, fax fax, cellphone cp, homeaddress ha, workaddress wa,
    website web, tags tags, birthday bd, notes notes
    FROM address
    WHERE coalesce(last,'') like ? and coalesce(first,'') like ?`
    db.all(q, [`%${req.body.ln}%`, `%${req.body.fn}%`], (err, rows) => {
      if (err) {
        res.render("addressshow", { 'prefix': prefix, 'rows': [] })
      } else {
        res.render("addressshow", { 'prefix': prefix, 'rows': rows, 'csrfToken': req.csrfToken() })
      }
    }
    )
  }
)
app.get("/getvcf",
  (req, res) => {
    const vcf = vCardJS()
    vcf.lastName = req.query.ln as string
    vcf.firstName = req.query.fn as string
    vcf.email = req.query.eml as string
    vcf.workPhone = req.query.wp as string
    vcf.homePhone = req.query.hp as string
    vcf.workFax = req.query.fax as string
    vcf.cellPhone = req.query.cp as string
    vcf.birthday = new Date(req.query.bd as string)
    vcf.note = req.query.notes as string
    res.set('Content-Type', 'text/vcard; name="' + req.query.ln + '.vcf"');
    res.set('Content-Disposition', 'inline; filename="' + req.query.ln + '.vcf"');
    res.send(vcf.getFormattedString());
  }
)
app.get("/lists",
  (req, res) => {
    db.all(
      `SELECT DISTINCT lst from lists`, (err, lists) => {
        res.render("lists", { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'lists': lists })
      }
    )
  }
)
app.get("/list",
  (req, res) => {
    const lst = req.query.lst
    db.all(
      `SELECT itm FROM lists WHERE lst = ?`, [lst], (err, itms) => {
        res.render("list", { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'lst': lst, 'itms': itms })
      })
  }
)
app.post("/newlist",
  parseForm,
  (req, res) => {
    db.run(
      `INSERT into lists values (?,?)`,
      [req.body.nlst, req.body.fitm],
      err => { console.log(err) }
    )
    res.redirect(prefix + "/lists")
  }
)
app.post("/listadditem",
  parseForm,
  (req, res) => {
    db.run(
      `INSERT INTO lists VALUES (?,?)`,
      [req.body.lst, req.body.nitm],
      err => {console.log(err)}
    )
    res.redirect(prefix + "/list?" + querystring.stringify({'lst': req.body.lst}))
  }
)
app.post("/listdelitem",
  parseForm,
  (req, res) => {
    db.run(
      `DELETE FROM lists WHERE lst = ? and itm = ?`,
      [req.body.lst, req.body.itm],
      err => {console.log(err)}
    )
    res.redirect(prefix + "/list?" + querystring.stringify({'lst': req.body.lst}))
  }
)
app.get("/bookmarks",
        (req,res) => {
          res.sendFile(bmPath);
        }
       )
app.get("/daily",
        (req,res) => {
          const myaml = yaml.safeLoad(fs.readFileSync(dailyPath, 'utf8'))
          res.render("daily", {'myaml': myaml, 'prefix': prefix})
        }
       )



// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮

// ==================== start the Express server ======================
app.listen(port, () => {
  console.log(`server started at http://localhost:${port}`);
});
