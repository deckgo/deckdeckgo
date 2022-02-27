import {Identity} from '@dfinity/agent';
import {LocalStorage} from '@dfinity/auth-client';
import {DelegationChain, DelegationIdentity, Ed25519KeyIdentity} from '@dfinity/identity';
import {InternetIdentityAuth} from '../types/identity';

export const internetIdentityAuth = async (): Promise<InternetIdentityAuth> => {
  const storage: LocalStorage = new LocalStorage('ic-');

  const identityKey: string | null = await storage.get('identity');
  const delegationChain: string | null = await storage.get('delegation');

  return {
    identityKey,
    delegationChain
  };
};

export const initIdentity = ({identityKey, delegationChain}: {identityKey: string | null; delegationChain: string | null}): Identity => {
  const chain: DelegationChain = DelegationChain.fromJSON(delegationChain);
  const key: Ed25519KeyIdentity = Ed25519KeyIdentity.fromJSON(identityKey);

  return DelegationIdentity.fromDelegation(key, chain);
};
