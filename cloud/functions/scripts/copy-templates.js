#!/usr/bin/env node

const fs = require('fs');

fs.copyFileSync('../../studio/src/assets/templates.json', './src/assets/templates.json');
