var http = require("http");
var fs = require("fs");
var net = require("net");
require("ical.js");

var hm = process.env.HOME;
var config = {};

function mkEvObj(event, derived) {
    var a = {"summary": event.summary, "isRecurring": event.isRecurring(), "isDerived": derived};
    if ("location" in event)  a.location = event.location;
    if ("duration" in event) a.duration = {
        "weeks": event.duration.weeks,
        "days": event.duration.days,
        "hours": event.duration.hours,
        "minutes": event.duration.minutes,
        "seconds": event.duration.seconds,
        "isNegative": event.duration.isNegative
    };
    if ("endDate" in event) a.endDate = {
        "year": event.endDate.year,
        "month": event.endDate.month,
        "day": event.endDate.day,
        "hour": event.endDate.hour,
        "minute": event.endDate.minute,
        "second": event.endDate.second,
        "isDate": event.endDate.isDate
    };
    if ("startDate" in event) a.startDate = {
        "year": event.startDate.year,
        "month": event.startDate.month,
        "day": event.startDate.day,
        "hour": event.startDate.hour,
        "minute": event.startDate.minute,
        "second": event.startDate.second,
        "isDate": event.startDate.isDate
    };
    if ("organizer" in event) a.organizer = event.organizer;
    return a;
}
function main(body,res) {
    console.log("BODY was:");
    console.log(body);
    console.log("---");
    var jbody = JSON.parse(body);
    var rFrom = new ICAL.Time (jbody.dateFrom);
    var rUntil = new ICAL.Time (jbody.dateUntil);
    fs.readFile(config.calendar, 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }
        var jcalData = ICAL.parse(data);
        var rootComponent = new ICAL.Component(jcalData);
        var components = rootComponent.getAllSubcomponents('vevent');
        //res.writeHead(200, {'Content-Type': 'text/plain'});
        for (i=0; i < components.length; i++) {
            var event = new ICAL.Event(components[i]);
            if ("startDate" in event) {
                var a = mkEvObj(event, false);
                if (event.startDate.compare(rFrom) >= 0 && event.startDate.compare(rUntil) <= 0) res.write(JSON.stringify(a));
                var recurator = event.iterator(event.startDate);
                recurator.next();
                while ((next = recurator.next()) && next.compare(rUntil) <= 0) {
                    if (next.compare(rFrom) < 0) continue; 
                    a.isDerived = true;
                    a.startDate = {
                        "year": next.year,
                        "month": next.month,
                        "day": next.day,
                        "hour": next.hour,
                        "minute": next.minute,
                        "second": next.second,
                        "isDate": next.isDate
                    }
                    var dur = event.duration;
                    next.addDuration(dur);
                    a.endDate = {
                        "year": next.year,
                        "month": next.month,
                        "day": next.day,
                        "hour": next.hour,
                        "minute": next.minute,
                        "second": next.second,
                        "isDate": next.isDate
                    }
                    res.write(JSON.stringify(a));
                }
            } else continue
        }
        res.end();
    });
}

server = http.createServer(function (request, response) {
    var body = [];
    request.on('data', function(chunk) {
        console.log("received CHUNK");
        body.push(chunk);
    }).on('end', function() {
        console.log("received END");
        body = Buffer.concat(body).toString();
        main (body, response);
    }); 
});

server.on('error', function (e) {
    // http://stackoverflow.com/questions/16178239/gracefully-shutdown-unix-socket-server-on-nodejs-running-under-forever
    if (e.code == 'EADDRINUSE') {
        var clientSocket = new net.Socket();
        clientSocket.on('error', function(e) { // handle error trying to talk to server
            if (e.code == 'ECONNREFUSED') {  // No other server listening
                fs.unlinkSync(config.socket);
                server.listen(config.socket, function() { //'listening' listener
                    console.log('server recovered');
                });
            }
        });
        clientSocket.connect({path: config.socket}, function() { 
            console.log('Server running, giving up...');
            process.exit();
        });
    }
});

fs.readFile(hm + '/.config/amkhlv/anode.json', 'utf8', function (err, data) {
    if (err) {return console.log(err);}
    config = JSON.parse(data);
    server.listen(config.socket);
})