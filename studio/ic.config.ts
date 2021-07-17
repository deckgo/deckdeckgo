// @ts-ignore
import {resolve} from 'path';

const config = (...pathSegments: string[]): Record<string, { ic?: string, local?: string }> => {
  try {
    return require(resolve(...pathSegments));
  } catch (err) {
    return {};
  }
}

export const canisterEnvIds = (prod: boolean): Record<string, string> => {
  const localCanisters = config('.dfx', 'local', 'canister_ids.json');
  const prodCanisters = config('canister_ids.json');

  const network = process.env.DFX_NETWORK || (prod ? 'ic' : 'local');

  const canisters = network === 'local' ? localCanisters : prodCanisters;

  return Object.entries(canisters).reduce((acc, [name, value]) => {
    const entry: Record<string, string> = {};
    entry[`process.env.${name.toUpperCase()}_CANISTER_ID`] = JSON.stringify(value[network]);

    return {
      ...acc,
      ...entry
    };
  }, {});
};
