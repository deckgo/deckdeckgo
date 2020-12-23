#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const crypto = require('crypto');

const configProd = require('../config.prod');
const configStaging = require('../config.staging');
const configDev = require('../config.dev');

const dev = process.argv && process.argv.indexOf('--dev') > -1;
const staging = process.argv && process.argv.indexOf('--staging') > -1;

function updateCSP(filename) {
  fs.readFile(`${filename}`, 'utf8', function (err, data) {
    if (err) {
      return console.log(err);
    }

    // 1. Replace API Url
    let result = data.replace(
      /<@API_URLS@>/g,
      `${dev ? configDev.API_URL : staging ? configStaging.API_URL : configProd.API_URL}/ ${
        dev ? configDev.UNSPLASH_URL : staging ? configStaging.UNSPLASH_URL : configProd.UNSPLASH_URL
      }`
    );

    // 2. Update service worker loader hash
    const swHash = findSWHash(data);
    if (swHash) {
      result = result.replace(/<@SW_LOADER@>/g, swHash);
    }

    fs.writeFile(`${filename}`, result, 'utf8', function (err) {
      if (err) return console.log(err);
    });
  });
}

function findSWHash(data) {
  const sw = /(<.?script data-build[\s\S]*?>)([\s\S]*?)(<\/script>)/gm;

  let m;
  while ((m = sw.exec(data))) {
    if (m && m.length >= 3 && m[2].indexOf('serviceWorker') > -1) {
      return `'sha256-${crypto.createHash('sha256').update(m[2]).digest('base64')}'`;
    }
  }

  return undefined;
}

function findHTMLFiles(dir, files) {
  fs.readdirSync(dir).forEach((file) => {
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
