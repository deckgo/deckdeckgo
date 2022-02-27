export interface Log {
  msg: string;
  duration?: number;
}

export const log = (detail: Log) => {
  const $event: CustomEvent<Log> = new CustomEvent<Log>('ddgLog', {detail, bubbles: true});
  document.dispatchEvent($event);
};
