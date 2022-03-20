import {busyBeforeUnload} from './before-unload.utils';

const throwBusy = (busy: boolean) => {
  const $event: CustomEvent<boolean> = new CustomEvent<boolean>('ddgBusy', {detail: busy, bubbles: true});
  document.dispatchEvent($event);
};

export const busy = (busy: boolean) => {
  busyBeforeUnload(busy);

  throwBusy(busy);
};
