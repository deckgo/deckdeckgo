#!/usr/bin/env node

import pkgAgent from '@dfinity/agent';
import {IDL} from '@dfinity/candid';
import pkgIdentity from '@dfinity/identity';
import pkgPrincipal from '@dfinity/principal';
import crypto from 'crypto';
import {readFileSync} from 'fs';
import fetch from 'node-fetch';
import {idlFactory} from '../../.dfx/local/canisters/manager/manager.did.mjs';

const {Principal} = pkgPrincipal;

const {Secp256k1KeyIdentity} = pkgIdentity;

const {HttpAgent, Actor} = pkgAgent;

const managerPrincipal = () => {
  const buffer = readFileSync('./canister_ids.json');
  const {manager} = JSON.parse(buffer.toString('utf-8'));
  return Principal.fromText(manager.ic);
};

/**
 * ! Replicating the dfx identity in a nodejs script is NOT possible at the moment !
 *
 * See: https://forum.dfinity.org/t/using-dfinity-agent-in-node-js/6169/41
 */
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

const loadWasm = (type) => {
  const buffer = readFileSync(`${process.cwd()}/.dfx/local/canisters/${type}/${type}.wasm`);
  return [...new Uint8Array(buffer)];
};

const fromNullable = (value) => {
  return value?.[0];
};

(async () => {
  const help = process.argv.find((arg) => arg.indexOf('--help') > -1)

  if (help !== undefined) {
    console.log('Options:');
    console.log('--type=data|storage');
    console.log('--list-only');
    return;
  }

  try {
    const canisterId = managerPrincipal();

    const identity = initIdentity();

    const agent = new HttpAgent({identity, fetch, host: 'https://ic0.app'});

    const actor = Actor.createActor(idlFactory, {
      agent,
      canisterId
    });

    // data or storage
    const type = process.argv.find((arg) => arg.indexOf('--type=') > -1)?.replace('--type=', '') ?? 'data';

    const list = await actor.list(type);

    // bucketId is optional in our backend
    const filterList = list.filter(({bucketId}) => fromNullable(bucketId) !== undefined);

    if (filterList.length <= 0) {
      console.log("No buckets found.");
      return;
    }

    const listOnly = process.argv.find((arg) => arg.indexOf('--list-only') > -1) !== undefined;

    if (listOnly) {
      console.log('List buckets:', filterList.map(({bucketId}) => bucketId[0].toText()));
      return;
    }


    // bucketId[0] -> effective bucketId
    // console.log(bucketId[0].toText());

    const wasmModule = loadWasm(type);

    const promises = filterList.map(({owner, bucketId}) => upgradeBucketData({actor, wasmModule, bucketId: fromNullable(bucketId), owner}));
    await Promise.all(promises);
  } catch (e) {
    console.error(e);
  }
})();
