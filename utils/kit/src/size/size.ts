import {isPapyrus, isScreenshot} from '../utils/utils.deck';
import {isFullscreen} from '@deckdeckgo/utils';

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
};

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

  const content: HTMLElement | null = document.querySelector('div.ion-page ion-content');

  const main: HTMLElement | null = document.querySelector('div.ion-page ion-content main');

  if (!content || !main) {
    return;
  }

  if (EMBEDDED || isPapyrus(deck) || isFullscreen() || isScreenshot()) {
    defaultSize(main);
  } else {
    aspectRatioSize({content, main});
  }

  await deck.initSlideSize();
};

const defaultSize = (main: HTMLElement) => {
  main.style.removeProperty('--main-size-width');
  main.style.removeProperty('--main-size-height');
};

const aspectRatioSize = ({content, main}: {content: HTMLElement; main: HTMLElement}) => {
  const maxHeight: number = content.offsetHeight - 32;

  const ratioWidth: number = content.offsetWidth - 192;
  const ratioHeight: number = (ratioWidth * 9) / 16;

  const {width, height}: Size =
    ratioHeight > maxHeight
      ? {
          width: `${(maxHeight * 16) / 9}px`,
          height: `${maxHeight}px`
        }
      : {
          width: `${ratioWidth}px`,
          height: `${ratioHeight}px`
        };

  main.style.setProperty('--main-size-width', width);
  main.style.setProperty('--main-size-height', height);
};
