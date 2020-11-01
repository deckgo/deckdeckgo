import {isMobile} from '@deckdeckgo/utils';
import {contrast} from '../utils/util.color';

export const initActions = async () => {
  const slider: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!slider) {
    return;
  }

  slider.addEventListener('slidesDidLoad', async () => {
    await initActionButtons();
    await initNavigation();
  });

  slider.addEventListener('slideNextDidChange', async () => {
    await initNavigation();
  });

  slider.addEventListener('slidePrevDidChange', async () => {
    await initNavigation();
  });

  slider.addEventListener('slideToChange', async () => {
    await initNavigation();
  });

  document.addEventListener('keydown', async ($event: KeyboardEvent) => {
    await handleTabOnKeydown($event, slider);
  });
};

async function initActionButtons() {
  const ionFab: HTMLElement | null = document.querySelector('ion-fab');

  if (ionFab) {
    const mobile = isMobile();

    if (mobile) {
      ionFab.style.setProperty('--deckgo-hide-on-mobile', 'none');
    }
  }
}

async function initNavigation() {
  const deck = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  const index = await deck.getActiveIndex();
  const begin = await deck.isBeginning();
  const end = await deck.isEnd();

  const slide = document.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

  if (!slide) {
    return;
  }

  const style: CSSStyleDeclaration = window.getComputedStyle(slide);
  const color: string = await contrast(style.backgroundColor);

  document.body.style.setProperty('--button-navigation-color', color);

  const prev = document.querySelector('button#previous') as HTMLElement | null;
  if (prev) {
    prev.style.opacity = begin ? '0' : '1';
  }

  const next = document.querySelector('button#next') as HTMLElement | null;
  if (next) {
    next.style.opacity = end ? '0' : '1';
  }
}

function handleTabOnKeydown($event: KeyboardEvent, slider: HTMLDeckgoDeckElement) {
  return new Promise(async (resolve) => {
    if (!$event || !slider) {
      resolve();
      return;
    }

    if ($event.code !== 'Tab') {
      resolve();
      return;
    }

    $event.preventDefault();

    if ($event.shiftKey) {
      await slider.slidePrev();
    } else {
      await slider.slideNext();
    }

    resolve();
  });
}
