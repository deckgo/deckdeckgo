import {Actor, ActorMethod, ActorSubclass, HttpAgent, Identity} from '@dfinity/agent';
import {IDL} from '@dfinity/candid';
import {Principal} from '@dfinity/principal';

export const createActor = async <T = Record<string, ActorMethod>>({
  canisterId,
  idlFactory,
  identity,
  host
}: {
  canisterId: string | Principal;
  idlFactory: IDL.InterfaceFactory;
  identity: Identity;
  host?: string;
}): Promise<ActorSubclass<T>> => {
  const agent = new HttpAgent({identity, ...(host && {host})});

  if (process.env.LOCAL_IDENTITY) {
    // Fetch root key for certificate validation during development
    await agent.fetchRootKey();
  }

  // Creates an actor with using the candid interface and the HttpAgent
  return Actor.createActor(idlFactory, {
    agent,
    canisterId
  });
};
