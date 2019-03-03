'use strict'

exports.handler = function(event, context, callback) {

    var fs = require('fs');
    var files = fs.readdirSync('.');

    var date = new Date();
    var current_hour = date.getHours();
    var current_minute = date.getMinutes();

    var body = "";

    try {
        var exec = require('child_process').exec;
        exec('./main_hs', (error, stdout, stderr) => {
            body += `<h1>${stdout}</h1>`;
            var response = {
                statusCode: 200,
                headers: {
                    'Content-Type': 'text/html; charset=utf-8'
                },
                body: body
            }
            callback(null, response)
        });
    }
    catch(error) {
        body += "\n<h2>ERROR: "
        body += error
        body += "</h2>"
        var response = {
            statusCode: 500,
            headers: {
                'Content-Type': 'text/html; charset=utf-8'
            },
            body: body
        }
        callback(null, response)
    }

}
