import {isDelegationValid} from '@dfinity/authentication';
import {DelegationChain} from '@dfinity/identity';
import type {InternetIdentityAuth} from '../types/identity';
import {SignOutWindow} from '../types/sync.window';

let idleTimer: boolean = false;

export const startIdleTime = async (
  {
    internetIdentity
  }: {
    internetIdentity: InternetIdentityAuth;
  },
  onSignOut: SignOutWindow
) => {
  idleTimer = true;

  while (idleTimer) {
    onIdleSignOut(internetIdentity, onSignOut);

    // Sleep. setInterval not supported - throw an error upon trying to use postmessage "onSignOut" callback
    await new Promise((r) => setTimeout(r, 5000));
  }
};

const stopTimer = () => (idleTimer = false);

export const stopIdleTimer = async () => stopTimer();

const onIdleSignOut = (internetIdentity: InternetIdentityAuth, onSignOut: SignOutWindow) => {
  const {delegationChain} = internetIdentity;

  if (delegationChain === null) {
    return;
  }

  if (isDelegationValid(DelegationChain.fromJSON(delegationChain))) {
    return;
  }

  // Clear timer to not emit sign-out multiple times
  stopTimer();

  onSignOut();
};
