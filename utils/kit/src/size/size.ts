import {isPapyrus} from '../utils/utils.deck';

interface Size {
  width: string;
  height: string;
}

export const initSize = async () => {
  if (window && 'ResizeObserver' in window) {
    initMainSizeObserver();
  } else {
    await initSizeOldBrowser();
  }
};

const initMainSizeObserver = () => {
  const mainResizeObserver: ResizeObserver = new ResizeObserver(async (_entries) => {
    await initMainSize();
  });

  const content: HTMLElement | null = document.querySelector('div.ion-page ion-content');

  if (content) {
    mainResizeObserver.observe(content);
  }
}

const initSizeOldBrowser = () => {
  if (window && 'ResizeObserver' in window) {
    return;
  }

  setTimeout(async () => await initMainSize(), 100);
};

const initMainSize = async () => {
  const deck = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  console.log(EMBEDDED, isPapyrus(deck));

  if (EMBEDDED || isPapyrus(deck)) {
    return;
  }

  const content: HTMLElement | null = document.querySelector('div.ion-page ion-content');

  const main: HTMLElement | null = document.querySelector('div.ion-page ion-content main');

  if (!content || !main) {
    return;
  }

  console.log(content, content.offsetHeight, content.offsetWidth);

  const {width, height}: Size = initEmbeddedSize(content);

  console.log(width, height);

  main.style.setProperty('--main-size-width', width);
  main.style.setProperty('--main-size-height', height);

  console.log(content);

  // TODO: missing header size

  await deck.initSlideSize();
};

const initEmbeddedSize = (content: HTMLElement): Size => {
  const maxHeight: number = content.offsetHeight - 32;

  const width: number = content.offsetWidth - 192;
  const height: number = (width * 9) / 16;

  return height > maxHeight
    ? {
        width: `${(maxHeight * 16) / 9}px`,
        height: `${maxHeight}px`,
      }
    : {
        width: `${width}px`,
        height: `${height}px`,
      };
};
