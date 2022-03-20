#!/usr/bin/env node

const {writeFileSync, readFileSync} = require('fs');

const copyAdminManagerMjs = ({src}) => {
  const buffer = readFileSync(`${src}manager/manager.did.js`);
  writeFileSync(`${src}manager/manager.did.mjs`, buffer.toString('utf-8'));
};

copyAdminManagerMjs({src: '.dfx/local/canisters/'});
