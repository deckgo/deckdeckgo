export interface Log {
  msg: string;
  level: 'info' | 'error';
  duration?: number;
}

export const log = (detail: Pick<Partial<Log>, 'level'> & Omit<Log, 'level'>) => {
  const logDetail: Log = {
    level: 'info',
    ...detail
  };

  const $event: CustomEvent<Log> = new CustomEvent<Log>('ddgLog', {detail: logDetail, bubbles: true});
  document.dispatchEvent($event);
};
