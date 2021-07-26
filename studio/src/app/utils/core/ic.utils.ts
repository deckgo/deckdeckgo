import {Actor, HttpAgent, HttpAgentOptions, ActorConfig, ActorMethod, ActorSubclass} from '@dfinity/agent';
import {IDL} from '@dfinity/candid';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../types/core/environment-config';

export const createActor = async <T = Record<string, ActorMethod>>({
  canisterId,
  idlFactory,
  agentOptions = {},
  actorConfig
}: {
  canisterId: string;
  idlFactory: IDL.InterfaceFactory;
  agentOptions?: HttpAgentOptions;
  actorConfig?: ActorConfig;
}): Promise<ActorSubclass<T>> => {
  const agent = new HttpAgent(agentOptions);

  if (EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app').mock) {
    // Fetch root key for certificate validation during development
    await agent.fetchRootKey();
  }

  // Creates an actor with using the candid interface and the HttpAgent
  return Actor.createActor(idlFactory, {
    agent,
    canisterId,
    ...(actorConfig && {actorConfig})
  });
};
