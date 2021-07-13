#!/usr/bin/env node

// Ensure all map files are deleted including those from Workbox

const {rmFiles} = require('./rm.utils.js');

rmFiles('./www/', '.map');
