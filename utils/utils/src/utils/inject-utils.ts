export function injectJS(id: string, src: string): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    if (document.getElementById(id)) {
      resolve('JS already loaded.');
      return;
    }
    const script = document.createElement('script');

    script.id = id;
    script.async = true;
    script.defer = true;
    script.src = src;

    script.addEventListener('load', () => resolve('JS loaded.'));

    script.addEventListener('error', () => reject('Error loading script.'));
    script.addEventListener('abort', () => reject('Script loading aborted.'));

    document.head.appendChild(script);
  });
}

export function injectCSS(id: string, src: string): Promise<string> {
  return new Promise<string>((resolve, reject) => {
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
