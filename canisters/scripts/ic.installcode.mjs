#!/usr/bin/env node

import {readFileSync} from 'fs';

import fetch from 'node-fetch';

import crypto from 'crypto';

import pkgPrincipal from '@dfinity/principal';
const {Principal} = pkgPrincipal;

import pkgIdentity from '@dfinity/identity';
const {Secp256k1KeyIdentity} = pkgIdentity;

import pkgAgent from '@dfinity/agent';
const {HttpAgent, Actor} = pkgAgent;

import {IDL} from '@dfinity/candid';

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

const upgradeBucketData = async ({actor, owner, bucketId, wasmModule}) => {
  console.log(`Upgrading: ${bucketId.toText()}`);

  const arg = IDL.encode([IDL.Principal], [owner]);

  await actor.installCode(bucketId, [...arg], wasmModule);

  console.log(`Done: ${bucketId.toText()}`);
};

const loadWasm = () => {
  const buffer = readFileSync(`${process.cwd()}/.dfx/local/canisters/data/data.wasm`);
  return [...new Uint8Array(buffer)];
};

const fromNullable = (value) => {
  return value?.[0];
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

    const list = await actor.list('data');

    // bucketId is optional in our backend
    const filterList = list.filter(({bucketId}) => fromNullable(bucketId) !== undefined);

    if (filterList.length <= 0) {
      return;
    }

    const wasmModule = loadWasm();

    // bucketId[0] -> effective bucketId
    // console.log(bucketId[0].toText());

    const promises = filterList.map(({owner, bucketId}) => upgradeBucketData({actor, wasmModule, bucketId: fromNullable(bucketId), owner}));
    await Promise.all(promises);
  } catch (e) {
    console.error(e);
  }
})();
