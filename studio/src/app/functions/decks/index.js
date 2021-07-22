import {Actor, HttpAgent} from '@dfinity/agent';

// Imports candid interface
import {idlFactory} from './decks.did.js';
// CANISTER_ID is replaced by webpack based on node enviroment
export const canisterId = process.env.DECKS_CANISTER_ID;

/**
 *
 * @param {string | Principal} canisterId Canister ID of Agent
 * @param {{agentOptions?: import("@dfinity/agent").HttpAgentOptions; actorOptions?: import("@dfinity/agent").ActorConfig}} [options]
 * @return {import("@dfinity/agent").ActorSubclass<import("./decks.did.js")._SERVICE>}
 */
export const createActor = (canisterId, options) => {
  const agent = new HttpAgent({...options?.agentOptions});

  // Fetch root key for certificate validation during development
  if (process.env.NODE_ENV !== 'production') agent.fetchRootKey();

  // Creates an actor with using the candid interface and the HttpAgent
  return Actor.createActor(idlFactory, {
    agent,
    canisterId,
    ...options?.actorOptions
  });
};

/**
 * A ready-to-use agent for the decks canister
 * @type {import("@dfinity/agent").ActorSubclass<import("./decks.did.js")._SERVICE>}
 */
export const decks = createActor(canisterId);
