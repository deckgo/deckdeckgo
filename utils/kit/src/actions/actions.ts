import {isMobile} from '@deckdeckgo/utils';

export const initActions = async () => {
  const slider: HTMLDeckgoDeckElement | null = document.getElementById('slider') as HTMLDeckgoDeckElement | null;

  if (!slider) {
    return;
  }

  slider.addEventListener('slidesDidLoad', async () => {
    await initActionButtons();
  });

  slider.addEventListener('slideNextDidChange', async () => {
    await initActionPlayPause(slider);
  });

  slider.addEventListener('slidePrevDidChange', async () => {
    await initActionPlayPause(slider);
  });

  slider.addEventListener('slideToChange', async () => {
    await initActionPlayPause(slider);
  });

  document.addEventListener('keydown', async ($event: KeyboardEvent) => {
    await handleTabOnKeydown($event, slider);
  });
};

function initActionButtons() {
  return new Promise(async (resolve) => {
    const ionFab: HTMLElement | null = document.querySelector('ion-fab');

    if (ionFab) {
      const mobile = isMobile();

      if (mobile) {
        ionFab.style.setProperty('--deckgo-hide-on-mobile', 'none');
      }

      // Workaround: https://github.com/deckgo/starter-kit/issues/31
      if (document.dir === 'rtl') {
        const ionFabList = ionFab.querySelector('ion-fab-list[side="start"]');
        if (ionFabList) {
          ionFabList.setAttribute('side', 'end');
        }

        ionFab.setAttribute('horizontal', 'start');
      }
    }

    resolve();
  });
}

export const initActionPlayPause = (deck: HTMLDeckgoDeckElement) => {
  return new Promise(async (resolve) => {
    const playButton = document.getElementById('play');
    const pauseButton = document.getElementById('pause');

    const index = await deck.getActiveIndex();

    const actionSlideElement = document.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

    if (
      !actionSlideElement ||
      (actionSlideElement.tagName !== 'deckgo-slide-youtube'.toUpperCase() && actionSlideElement.tagName !== 'deckgo-slide-video'.toUpperCase())
    ) {
      if (playButton) {
        playButton.style.display = 'none';
      }

      if (pauseButton) {
        pauseButton.style.display = 'none';
      }

      resolve();
      return;
    }

    if (playButton) {
      playButton.style.display = 'initial';
    }

    if (pauseButton) {
      pauseButton.style.display = 'none';
    }

    resolve();
  });
};

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
