import {lazyLoadSelectedLazyImagesComponent} from '@deckdeckgo/utils';

// Workaround issue https://github.com/ionic-team/ionic-framework/issues/24173
export const printDoc = ({element}: {element: Node}) => {
  const body: HTMLBodyElement | null = document.querySelector('body');

  if (!body) {
    return;
  }

  const appRoot: HTMLElement | null = body.querySelector('app-root');
  let node: Node | null | undefined = undefined;

  window.addEventListener(
    'afterprint',
    () => {
      if (node) {
        body.removeChild(node);
      }

      appRoot?.classList.remove('hidden');
    },
    {once: true}
  );

  const onRender = async (_mutations: MutationRecord[], observer: MutationObserver) => {
    observer.disconnect();

    const images: NodeListOf<HTMLDeckgoLazyImgElement> = (node as HTMLElement).querySelectorAll('deckgo-lazy-img');
    images.forEach((img: HTMLDeckgoLazyImgElement) => (img.customLoader = true));
    await lazyLoadSelectedLazyImagesComponent(Array.from(images));

    appRoot?.classList.add('hidden');

    setTimeout(() => window.print(), 500);
  };

  const docObserver: MutationObserver = new MutationObserver(onRender);
  docObserver.observe(body, {childList: true, subtree: true});

  node = body?.appendChild(element.cloneNode(true));
};
