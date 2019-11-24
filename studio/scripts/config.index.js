#!/usr/bin/env node

const fs = require('fs');

const configProd = require('../config.prod');
const configDev = require('../config.dev');

const dev = process.argv && process.argv.indexOf('--dev') > -1;

// https://stackoverflow.com/a/14181136/5404186
function updateIndexHml(filename) {
    fs.readFile(`./www/${filename}`, 'utf8', function (err, data) {
        if (err) {
            return console.log(err);
        }

        const result = data.replace(/<@API_URL@>/g, dev ? configDev.API_URL : configProd.API_URL);

        fs.writeFile(`./www/${filename}`, result, 'utf8', function (err) {
            if (err) return console.log(err);
        });
    });
}

updateIndexHml('index.html');

if (!dev) {
    updateIndexHml('index-org.html');
}
