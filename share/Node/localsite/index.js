"use strict";
exports.__esModule = true;
var express = require("express");
var path = require("path");
var yaml = require("js-yaml");
var fs = require("fs");
var os = require("os");
var passport = require("passport");
var passportLocal = require("passport-local");
var LocalStrategy = passportLocal.Strategy;
var session = require("express-session");
var bodyParser = require("body-parser");
var csrf = require("csurf");
var crypto = require("crypto");
var sql = require("sqlite3");
var sqlStore = require("connect-sqlite3");
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
var conf = yaml.safeLoad(fs.readFileSync(path.join(os.homedir(), ".config/amkhlv/localsite/config.yaml"), "utf8"));
var mainPage = '/';
var calPath = path.join(conf.workingPath, conf.calendarFile);
var musPath = path.join(conf.workingPath, conf.musicFile);
var prefix = conf.prefix;
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ===================== config =====================================
var sessionSecret = conf.sessionSecret;
var port = conf.port; // default port to listen
var User = /** @class */ (function () {
    function User() {
    }
    return User;
}());
var users = conf.users;
var logins = users.map(function (u) { return u.login; });
var db = new sql.Database(path.join(conf.workingPath, conf.sqliteFile));
var SQLiteStore = sqlStore(session);
// ====================== App init ===================================
var app = express();
app.set("views", path.join(__dirname, "views"));
app.set("view engine", "ejs");
// ====================== Passport.js config =========================
passport.use(new LocalStrategy(function (username, password, done) {
    if (logins.includes(username)) {
        var a = users.find(function (u) {
            var h = crypto.scryptSync(password, u.salt, 64).toString('hex');
            return (u.login === username) && (h === u.hash);
        });
        if (a === undefined) {
            return done(null, false, { message: "wrong password" });
        }
        else {
            return done(null, username);
        }
    }
    else {
        return done(null, false, { message: "no such user" });
    }
}));
passport.serializeUser(function (user, cb) {
    cb(null, user);
});
passport.deserializeUser(function (id, cb) {
    if ((typeof id === 'string') && (logins.includes(id))) {
        cb(null, id);
    }
    else {
        cb('ERROR: User Not Found...');
    }
});
var parseForm = bodyParser.urlencoded({ extended: false });
app.use(parseForm);
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
var csrfProtection = csrf({ cookie: false });
// const csrfProtection = csrf({ cookie: true }) // <-- WRONG !
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ====================== Static files are in folder public/ =========
app.use(csrfProtection);
app.use(express.static(conf.staticPath));
app.use(function (err, req, res, next) {
    if (err.code !== 'EBADCSRFTOKEN')
        return next(err);
    // handle CSRF token errors here
    res.status(403);
    res.send('form tampered with');
});
// ====================== Authentication routes ======================
app.get('/login', function (req, res) {
    res.render('login', { csrfToken: req.csrfToken(), 'prefix': prefix });
});
app.post('/login', parseForm, passport.authenticate('local', { failureRedirect: prefix + '/login' }), function (req, res) {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log("AUTH OK");
    res.redirect(prefix + mainPage);
});
app.get('/logout', function (req, res) {
    req.logout();
    res.redirect(prefix + '/');
});
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
function checkPWD(ruser) {
    return (typeof ruser === 'string') && (ruser.length > 2) && (logins.includes(ruser));
}
app.get("/", function (req, res) {
    if (checkPWD(req.user)) {
        console.log("-- allowing " + req.user);
        res.render("index", { 'homedir': path.join(os.homedir()).toString(), 'prefix': prefix });
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed");
        res.redirect(prefix + '/login');
    }
});
app.get("/music", function (req, res) {
    if (checkPWD(req.user)) {
        console.log("-- allowing " + req.user + " to music");
        var myaml = yaml.safeLoad(fs.readFileSync(musPath, 'utf8'));
        res.render("bookmarks", { 'ttl': 'Music', 'myaml': myaml, 'ncols': 3, 'prefix': prefix });
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to /music");
        res.redirect(prefix + '/login');
    }
});
app.get("/calendar", function (req, res) {
    if (checkPWD(req.user)) {
        res.sendFile(calPath);
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to /calendar");
        res.redirect(prefix + '/login');
    }
});
app.get("/todo", function (req, res) {
    if (checkPWD(req.user)) {
        var todos = db.all("select todo td, note nt, importance im from todolist", function (err, rows) {
            res.render("todolist", { 'prefix': prefix, 'csrfToken': req.csrfToken(), 'rows': rows });
        });
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to /todo");
        res.redirect(prefix + '/login');
    }
});
app.post("/tododelete", parseForm, function (req, res) {
    if (checkPWD(req.user)) {
        console.log(JSON.stringify(req.body, null, 2));
        db.run("delete from todolist where todo = ? and note = ? and importance = ?", [req.body.td, req.body.nt, req.body.im], function (err) { });
        res.redirect(prefix + '/todo');
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to delete a TODO");
        res.redirect(prefix + '/login');
    }
});
app.post("/todoadd", parseForm, function (req, res) {
    if (checkPWD(req.user)) {
        console.log(JSON.stringify(req.body, null, 2));
        db.run("insert into todolist values (?,?,?)", [req.body.td, req.body.nt, req.body.im], function (err) { });
        res.redirect(prefix + '/todo');
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to delete a TODO");
        res.redirect(prefix + '/login');
    }
});
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ==================== start the Express server ======================
app.listen(port, function () {
    console.log("server started at http://localhost:" + port);
});
