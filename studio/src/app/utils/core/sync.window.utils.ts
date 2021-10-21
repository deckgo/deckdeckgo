import {SyncState} from '@deckdeckgo/editor';

const onBeforeUnload = ($event: BeforeUnloadEvent) => {
  $event.preventDefault();
  return ($event.returnValue = 'Are you sure you want to exit?');
};

const addSyncBeforeUnload = () => {
  window.addEventListener('beforeunload', onBeforeUnload, {capture: true});
};

export const removeSyncBeforeUnload = () => {
  window.removeEventListener('beforeunload', onBeforeUnload, {capture: true});
};

export const syncBeforeUnload = (sync: SyncState) => {
  if (['pending', 'in_progress'].includes(sync)) {
    addSyncBeforeUnload();
  } else {
    removeSyncBeforeUnload();
  }
};
