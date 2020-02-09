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
var crypto = require("crypto");
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: Project Specific Header ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
var conf = yaml.safeLoad(fs.readFileSync(path.join(os.homedir(), ".config/amkhlv/localsite/config.yaml"), "utf8"));
var mainPage = '/music';
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
app.use(bodyParser.urlencoded({ extended: false }));
app.use(session({ secret: sessionSecret, resave: false, saveUninitialized: false }));
app.use(passport.initialize());
app.use(passport.session());
// ====================== Static files are in folder public/ =========
app.use(express.static('public'));
// ====================== Authentication routes ======================
app.get('/login', function (req, res) {
    res.render('login');
});
app.post('/login', passport.authenticate('local', { failureRedirect: '/login' }), function (req, res) {
    // If this function gets called, authentication was successful.
    // `req.user` contains the authenticated user.
    console.log("AUTH OK");
    res.redirect(mainPage);
});
app.get('/logout', function (req, res) {
    req.logout();
    res.redirect('/');
});
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ START: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
app.get("/", function (req, res) {
    // render the index template
    res.render("index");
});
app.get("/music", function (req, res) {
    if ((typeof req.user === 'string') && (logins.includes(req.user))) {
        console.log("-- allowing " + req.user + " to music");
        var myaml = yaml.safeLoad(fs.readFileSync(conf.music, 'utf8'));
        var genres = Object.keys(myaml);
        res.render("bookmarks", { 'ttl': 'Music', 'myaml': myaml, 'ncols': 3 });
    }
    else {
        console.log("USER>>>" + req.user + "<<< not allowed to /music");
        res.redirect('/login');
    }
});
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮ END: project specific routes ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮▮
// ==================== start the Express server ======================
app.listen(port, function () {
    console.log("server started at http://localhost:" + port);
});
