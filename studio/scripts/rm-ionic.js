#!/usr/bin/env node

// Delete unused Ionic assets and dependencies from output bundle

const {rmDir} = require('./rm.utils.js');

rmDir('www/build/svg');
rmDir('www/build/swiper');
