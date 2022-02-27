export const throwError = (error: string) => {
  const $event: CustomEvent<string> = new CustomEvent<string>('ddgError', {detail: error, bubbles: true});
  document.dispatchEvent($event);
};
