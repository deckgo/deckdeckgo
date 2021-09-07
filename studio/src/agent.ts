import {Actor, HttpAgent} from '@dfinity/agent';

// @ts-ignore
import {idlFactory as counter_idl, canisterId as counter_id} from 'dfx-generated/counter';

const agentOptions = {
  host: 'http://localhost:8000'
};

const agent = new HttpAgent(agentOptions);
const counter = Actor.createActor(counter_idl, {agent, canisterId: counter_id});

export {counter};
