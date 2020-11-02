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
  const mobile = isMobile();

  const ionFab: HTMLElement | null = document.querySelector('ion-fab');

  if (ionFab) {
    if (mobile) {
      ionFab.style.setProperty('--deckgo-hide-on-mobile', 'none');
    }
  }

  const deck = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!deck) {
    return;
  }

  if (isPapyrus(deck)) {
    const content: HTMLElement | null = document.querySelector('div.ion-page');
    content?.classList.add('papyrus');

    if (window && 'IntersectionObserver' in window) {
      const firstSlide = document.querySelector('.deckgo-slide-container:nth-child(1)') as HTMLElement | null;

      if (!firstSlide) {
        return;
      }

      const observer = new IntersectionObserver(handlePapyrusScroll, {
        threshold: 0.75,
      });

      observer.observe(firstSlide as Element);
    }

    return;
  }

  if (isVertical(deck)) {
    const content: HTMLElement | null = document.querySelector('div.ion-page');
    content?.classList.add('vertical');
  }
}

function handlePapyrusScroll(entries: IntersectionObserverEntry[]) {
  if (!entries || entries.length <= 0) {
    return;
  }

  const next = document.querySelector('button#next') as HTMLElement | null;

  if (!next) {
    return;
  }

  next.style.opacity = entries[0].isIntersecting ? '1' : '0';
}

function isPapyrus(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'papyrus' && !mobile) || (deck.directionMobile === 'papyrus' && mobile);
}

function isVertical(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'vertical' && !mobile) || (deck.directionMobile === 'vertical' && mobile);
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
