import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

export interface InjectScript {
  id: string;
  src: string;
  module?: boolean;
}

export class Utils {
  static injectJS(scr: InjectScript): Promise<string> {
    return new Promise<string>((resolve, reject) => {
      if (!document) {
        resolve();
        return;
      }

      if (document.getElementById(scr.id)) {
        resolve('JS already loaded.');
        return;
      }
      const script = document.createElement('script');

      script.id = scr.id;
      script.async = true;
      script.defer = true;
      script.src = scr.src;

      if (scr.module) {
        script.type = 'module';
      }

      script.addEventListener('load', () => resolve('JS loaded.'));

      script.addEventListener('error', () => reject('Error loading script.'));
      script.addEventListener('abort', () => reject('Script loading aborted.'));

      document.head.appendChild(script);
    });
  }

  static injectCSS(id: string, src: string): Promise<string> {
    return new Promise<string>((resolve, reject) => {
      if (!document) {
        resolve();
        return;
      }

      if (document.getElementById(id)) {
        resolve('CSS already loaded.');
        return;
      }

      const link = document.createElement('link');
      link.id = id;
      link.setAttribute('rel', 'stylesheet');
      link.setAttribute('href', src);

      link.addEventListener('load', () => resolve('CSS loaded.'));

      link.addEventListener('error', () => reject('Error loading css.'));
      link.addEventListener('abort', () => reject('CSS loading aborted.'));
      document.head.appendChild(link);
    });
  }

  static getNow(): Promise<string> {
    return new Promise<string>((resolve) => {
      const options: DateTimeFormatOptions = {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false,
      };
      const now: string = new Intl.DateTimeFormat('en-US', options).format(new Date());

      resolve(now.replace(/,/g, '').replace(/:/g, '-'));
    });
  }
}
