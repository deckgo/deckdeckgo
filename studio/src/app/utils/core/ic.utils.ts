import {Actor, HttpAgent, ActorMethod, ActorSubclass, Identity} from '@dfinity/agent';
import {IDL} from '@dfinity/candid';
import {Principal} from '@dfinity/principal';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../types/core/environment-config';

export const createActor = async <T = Record<string, ActorMethod>>({
  canisterId,
  idlFactory,
  identity
}: {
  canisterId: string | Principal;
  idlFactory: IDL.InterfaceFactory;
  identity: Identity;
}): Promise<ActorSubclass<T>> => {
  const agent = new HttpAgent({identity});

  if (EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app').mock) {
    // Fetch root key for certificate validation during development
    await agent.fetchRootKey();
  }

  // Creates an actor with using the candid interface and the HttpAgent
  return Actor.createActor(idlFactory, {
    agent,
    canisterId
  });
};
