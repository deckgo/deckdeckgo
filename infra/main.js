'use strict'

exports.handler = function(event, context, callback) {

    try {
        var execFile = require('child_process').execFile;
        execFile('./main_hs', {input: "hello"}, (error, stdout, stderr) => {
            callback(null, JSON.parse(stdout))
        });

    }
    catch(error) {
        var response = {
            statusCode: 500,
            headers: {
                'Content-Type': 'text/html; charset=utf-8'
            },
            body: "<h2>There was an error :( </h2>"
        }
        callback(null, response)
    }
}
