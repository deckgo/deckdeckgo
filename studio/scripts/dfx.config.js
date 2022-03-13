#!/usr/bin/env node

const {resolve} = require('path');

const config = (...pathSegments) => {
  try {
    return require(resolve('../', ...pathSegments));
  } catch (err) {
    return {};
  }
};

const canisterLocalInternetId = (network) => {
  if (network !== 'local') {
    return {};
  }

  try {
    const wallets = require(resolve('../', '.dfx', 'local', 'wallets.json'));

    return {'process.env.LOCAL_IDENTITY_CANISTER_ID': JSON.stringify(wallets.identities.default.local)};
  } catch (err) {
    return {};
  }
};

const canisterEnvIds = (prod) => {
  const localCanisters = config('.dfx', 'local', 'canister_ids.json');
  const prodCanisters = config('canister_ids.json');

  const network = prod ? 'ic' : 'local';

  const canisters = network === 'local' ? localCanisters : prodCanisters;

  return Object.entries(canisters).reduce(
    (acc, [name, value]) => {
      const entry = {};
      entry[`process.env.${name.toUpperCase()}_CANISTER_ID`] = JSON.stringify(value[network]);

      return {
        ...acc,
        ...entry
      };
    },
    {
      ...canisterLocalInternetId(network),
      'process.env.LOCAL_IDENTITY': `${network === 'local'}`
    }
  );
};

console.log('Local canister IDs:', canisterEnvIds(false));
console.log('Prod canister IDs:', canisterEnvIds(true));
