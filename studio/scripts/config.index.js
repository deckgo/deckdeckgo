#!/usr/bin/env node

const fs = require('fs');

const crypto = require('crypto');

const configProd = require('../config.prod');
const configDev = require('../config.dev');

const dev = process.argv && process.argv.indexOf('--dev') > -1;

function updateCSP(filename) {
    fs.readFile(`./www/${filename}`, 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }

        // 1. Replace API Url
        let result = data.replace(/<@API_URL@>/g, dev ? configDev.API_URL : configProd.API_URL);

        // 2. Update service worker loader hash
        const swHash = findSWHash(data);
        if (swHash) {
            result = result.replace(/<@SW_LOADER@>/g, swHash);
        }

        // 3. Update CSS link until https://github.com/ionic-team/stencil/issues/2039 solved
        result = result.replace(/rel=stylesheet media="\(max-width: 0px\)" importance=low onload="this\.media=''"/g, 'rel=stylesheet importance=low');

        fs.writeFile(`./www/${filename}`, result, 'utf8', function (err) {
            if (err) return console.log(err);
        });
    });
}

function findSWHash(data) {
    const sw = /(<.?script data-build.*?>)([\s\S]*?)(<\/script>)/gm;

    let m;
    while (m = sw.exec(data)) {
        if (m && m.length >= 3 && m[2].indexOf('serviceWorker') > -1) {
            return `'sha256-${crypto.createHash('sha256').update(m[2]).digest('base64')}'`;
        }
    }

    return undefined;
}

updateCSP('index.html');

if (!dev) {
    updateCSP('index-org.html');
}
