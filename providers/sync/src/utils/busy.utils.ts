export const throwBusy = (busy: boolean) => {
  const $event: CustomEvent<boolean> = new CustomEvent<boolean>('ddgBusy', {detail: busy, bubbles: true});
  document.dispatchEvent($event);
};
