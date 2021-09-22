// @ts-ignore
import {resolve} from 'path';

const config = (...pathSegments: string[]): Record<string, {ic?: string; local?: string}> => {
  try {
    return require(resolve('../', '../', 'canisters', ...pathSegments));
  } catch (err) {
    return {};
  }
};

const canisterLocalInternetId = (network: 'local' | string): Record<string, string> => {
  if (network !== 'local') {
    return {};
  }

  try {
    const wallets: {identities: {default: {local: string}}} = require(resolve('../', '../', 'canisters', '.dfx', 'local', 'wallets.json'));

    return {'process.env.LOCAL_IDENTITY_CANISTER_ID': JSON.stringify(wallets.identities.default.local)};
  } catch (err) {
    return {};
  }
};

export const canisterEnvIds = (prod: boolean): Record<string, string> => {
  const localCanisters = config('.dfx', 'local', 'canister_ids.json');
  const prodCanisters = config('canister_ids.json');

  const network = prod ? 'ic' : 'local';

  const canisters = network === 'local' ? localCanisters : prodCanisters;

  return Object.entries(canisters).reduce(
    (acc, [name, value]) => {
      const entry: Record<string, string> = {};
      entry[`process.env.${name.toUpperCase()}_CANISTER_ID`] = JSON.stringify(value[network]);

      return {
        ...acc,
        ...entry
      };
    },
    {...canisterLocalInternetId(network), 'process.env.LOCAL_IDENTITY': `${network === 'local'}`}
  );
};
