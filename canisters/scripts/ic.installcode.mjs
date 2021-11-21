import {readFileSync} from 'fs';

import fetch from 'node-fetch';

import crypto from 'crypto';

import pkgPrincipal from '@dfinity/principal';
const {Principal} = pkgPrincipal;

import pkgIdentity from '@dfinity/identity';
const {Secp256k1KeyIdentity} = pkgIdentity;

import pkgAgent from '@dfinity/agent';
const {HttpAgent, Actor} = pkgAgent;

import {idlFactory} from '../../.dfx/local/canisters/manager/manager.did.mjs';

const managerPrincipal = () => {
  const buffer = readFileSync('./canister_ids.json');
  const {manager} = JSON.parse(buffer.toString('utf-8'));
  return Principal.fromText(manager.ic);
};

const initIdentity = () => {
  const buffer = readFileSync('/Users/daviddalbusco/.config/dfx/identity/default/identity.pem');
  const key = buffer.toString('utf-8');

  const privateKey = crypto.createHash('sha256').update(key).digest('base64');

  return Secp256k1KeyIdentity.fromSecretKey(Buffer.from(privateKey, 'base64'));
};

(async () => {
  try {
    const canisterId = managerPrincipal();

    const identity = initIdentity();

    const agent = new HttpAgent({identity, fetch, host: 'https://ic0.app'});

    const actor = Actor.createActor(idlFactory, {
      agent,
      canisterId
    });

    const buffer = readFileSync(`${process.cwd()}/.dfx/local/canisters/data/data.wasm`);
    await actor.installCodeData([...new Uint8Array(buffer)]);
  } catch (e) {
    console.error(e);
  }
})();
