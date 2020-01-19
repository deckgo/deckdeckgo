#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const crypto = require('crypto');

const configProd = require('../config.prod');
const configDev = require('../config.dev');

const dev = process.argv && process.argv.indexOf('--dev') > -1;

function updateCSP(filename) {
    fs.readFile(`${filename}`, 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }

        // 1. Replace API Url
        let result = data.replace(/<@API_URLS@>/g, `${dev ? configDev.API_URL : configProd.API_URL}/ ${dev ? configDev.UNSPLASH_URL : configProd.UNSPLASH_URL}`);

        // 2. Update service worker loader hash
        const swHash = findSWHash(data);
        if (swHash) {
            result = result.replace(/<@SW_LOADER@>/g, swHash);
        }

        // 3. Update CSS link until https://github.com/ionic-team/stencil/issues/2039 solved
        result = result.replace(/rel=stylesheet media="\(max-width: 0px\)" importance=low onload="this\.media=''"/g, 'rel=stylesheet importance=low');

        // 4. Update preloadmodule hash in the CSP rules, this will speed up the loading (even if Audit is still displaying a warning)
        const linksHash = findPreloadModuleLinksHash(result);
        if (linksHash) {
            result = result.replace(/<@PRELOADMODULE_LINKS@>/g, linksHash);
        }

        fs.writeFile(`${filename}`, result, 'utf8', function (err) {
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

function findPreloadModuleLinksHash(data) {
    const preload = /(<.?link (rel=\"modulepreload\"|rel=modulepreload).*?>)/gm;

    const shas = [];

    let m;
    while (m = preload.exec(data)) {
        shas.push(`'sha256-${crypto.createHash('sha256').update(m[0]).digest('base64')}'`);
    }

    return shas && shas.length > 0 ? shas.join(' ') : undefined;
}

function findHTMLFiles(dir, files) {
    fs.readdirSync(dir).forEach(file => {
        const fullPath = path.join(dir, file);
        if (fs.lstatSync(fullPath).isDirectory()) {
            findHTMLFiles(fullPath, files);
        } else if (path.extname(fullPath) === '.html') {
            files.push(fullPath);
        }
    });
}

let htmlFiles = [];
findHTMLFiles('./www/', htmlFiles);

for (const file of htmlFiles) {
    updateCSP(`./${file}`);
}
