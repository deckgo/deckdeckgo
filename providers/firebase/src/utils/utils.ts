// TODO Refactor

export interface InjectScript {
  id: string;
  src: string;
  module?: boolean;
}

export class Utils {
  static injectJS(scr: InjectScript): Promise<string> {
    return new Promise<string>((resolve, reject) => {
      if (document?.getElementById(scr.id)) {
        resolve('JS already loaded.');
        return;
      }
      const script = document.createElement('script');

      script.id = scr.id;
      script.src = scr.src;

      if (scr.module) {
        script.type = 'module';
      } else {
        script.async = true;
        script.defer = true;
      }

      script.addEventListener('load', () => resolve('JS loaded.'), {once: true});

      script.addEventListener('error', () => reject('Error loading script.'), {once: true});
      script.addEventListener('abort', () => reject('Script loading aborted.'), {once: true});

      document.head.appendChild(script);
    });
  }

  static injectCSS(id: string, src: string): Promise<string> {
    return new Promise<string>((resolve, reject) => {
      if (document?.getElementById(id)) {
        resolve('CSS already loaded.');
        return;
      }

      const link = document.createElement('link');
      link.id = id;
      link.setAttribute('rel', 'stylesheet');
      link.setAttribute('href', src);

      link.addEventListener('load', () => resolve('CSS loaded.'), {once: true});

      link.addEventListener('error', () => reject('Error loading css.'), {once: true});
      link.addEventListener('abort', () => reject('CSS loading aborted.'), {once: true});
      document.head.appendChild(link);
    });
  }
}
