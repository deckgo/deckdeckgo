import {EventEmitter} from '@stencil/core';

import {deckdeckgoHighlightCodeLanguages} from '../declarations/deckdeckgo-highlight-code-languages';

export type StateRequiredJS = 'loaded' | 'attached' | 'error' | 'abort';

export const injectRequiredJS = ({lang}: {lang: string}): Promise<StateRequiredJS> => {
  return new Promise<StateRequiredJS>((resolve, reject) => {
    let script: HTMLScriptElement | null = document.querySelector(`deckdeckgo-prism-${lang}`);

    if (script) {
      resolve(script.hasAttribute('deckdeckgo-prism-loaded') ? 'loaded' : 'attached');
      return;
    }

    script = document.createElement('script');

    script.setAttribute('deckdeckgo-prism', lang);
    script.defer = true;
    script.src = scriptSrc(lang);

    script.addEventListener('load', () => {
      script.setAttribute('deckdeckgo-prism-loaded', lang);
      resolve('loaded');
    });

    script.addEventListener('error', () => reject('error'));
    script.addEventListener('abort', () => reject('abort'));

    document.head.appendChild(script);
  });
};

export const loadMainScript = ({
  lang,
  reload = false,
  prismLanguageLoaded
}: {
  lang: string;
  reload?: boolean;
  prismLanguageLoaded: EventEmitter<string>;
}): Promise<void> => {
  return new Promise<void>(async (resolve) => {
    if (!document || !lang || lang === '') {
      resolve();
      return;
    }

    // No need to load javascript, it is there
    if (lang === 'javascript') {
      prismLanguageLoaded.emit('javascript');

      resolve();
      return;
    }

    const scripts = document.querySelector("[deckdeckgo-prism='" + lang + "']");
    if (scripts) {
      if (reload) {
        prismLanguageLoaded.emit(lang);
      }

      resolve();
      return;
    }

    const script: HTMLScriptElement = document.createElement('script');

    script.onload = () => {
      script.setAttribute('deckdeckgo-prism-loaded', lang);
      prismLanguageLoaded.emit(lang);
    };

    script.onerror = () => {
      if (script.parentElement) {
        script.parentElement.removeChild(script);
      }

      // if the language definition doesn't exist or if unpkg is down, display code anyway
      prismLanguageLoaded.emit(lang);
    };

    const definition = deckdeckgoHighlightCodeLanguages[lang];
    const language = definition.main ? definition.main : lang;

    script.src = scriptSrc(language);
    script.setAttribute('deckdeckgo-prism', language);
    script.defer = true;

    document.head.appendChild(script);

    script.addEventListener('load', () => resolve(), {once: true});
  });
};

const scriptSrc = (language: string): string => {
  return 'https://unpkg.com/prismjs@latest/components/prism-' + language + '.js';
};