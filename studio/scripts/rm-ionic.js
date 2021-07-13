#!/usr/bin/env node

// Delete unused Ionic assets and dependencies from output bundle

const {rm} = require('./rm.utils.js');

rm('www/build/svg');
rm('www/build/swiper');
