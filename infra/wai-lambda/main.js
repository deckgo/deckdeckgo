'use strict'

const execFile = require('child_process').execFile;
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const os = require('os');

let child = null;

// Retrieve the singleton Haskell process
function getHaskellProcess() {

    if (child == null) {

        child = execFile(path.join(process.env['LAMBDA_TASK_ROOT'],'main_hs'));

        console.log('Exec-ed child');

        // Set proper encoding
        child.stdin.setEncoding('utf-8');
        child.stdout.setEncoding('utf-8');

        // Forward Haskell logs to CloudWatch
        child.stdout.on('data', function(data) {
            console.log('child: ' + data);
        });

        child.stderr.on('data', function(data) {
            console.log('child err: ' + data);
        });

        // Crash if Haskell process returned
        child.on('exit', function() {
            console.log("child returned!");
            process.exit(1);
        });

        console.log('Child is ready');
    }

    return child;
}

exports.handler = function(event, context, callback) {

    // keeping the child (Haskell) process around means that the node event
    // loop is never really empty. Setting this means that Lambda won't wait
    // around for something that'll never happen.
    context.callbackWaitsForEmptyEventLoop = false;

    // Get a new response ID and directory
    const respId = crypto.randomBytes(20).toString('hex');
    const responseDirTemplate = path.join(os.tmpdir(), 'resp-');

    fs.mkdtemp(responseDirTemplate, (err, responseDir) => {

        if (err) throw err;

        const responseFile = path.join(responseDir, 'response.json');

        console.log("Expecting answer in " + responseDir);

        let fswatcher = null;
        let fileWasHandled = false;

        fswatcher = fs.watch(responseDir, (eventType, filename) => {

            console.log("Got event: " + eventType + " at " + filename);

            // my gut feeling is that there could be a race condition if a
            // thread yields _after_ the eval of "!fileWasHandled" and _before_
            // setting "fileWasHandled = true", though not 100% sure since node
            // is single threaded and I have no idea if any thread can just
            // "yield"
            if (eventType === 'rename' && filename === 'response.json' && !fileWasHandled) {

                // prevent other events from trying to handle the file
                fileWasHandled = true;

                // no need to watch this file anymore
                fswatcher.close();

                console.log("Event is response");

                // Parse the response from the Haskell process
                const response = JSON.parse(fs.readFileSync(responseFile, 'utf8'));
                console.log("Parsed response: " + JSON.stringify(response));

                // Clean up the directory and finally reply
                console.log("cleaning up");
                fs.unlink(responseFile, (err) => {
                    if (err) throw err;
                    fs.rmdir(responseDir);
                });
                callback(null, response);
            }
        });

        // Send the request to the child
        const req = { responseFile: responseFile, request: event }
        const reqStr = JSON.stringify(req);
        console.log("Writing request: " + reqStr);

        getHaskellProcess().stdin.write(reqStr + "\n");
        console.log("Request was sent to Haskell process.");
    });
}
