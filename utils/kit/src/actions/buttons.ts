import {videoStart, videoPause} from './play-pause';
import {previousSlide, nextSlide, toggleFullScreen} from './slider';
import {openRemote} from '../modals/remote';

export const initButtons = () => {
  if (!document) {
    return;
  }

  document.addEventListener('DOMContentLoaded', async () => {
    attachClickListener('play', videoStart);
    attachClickListener('pause', videoPause);
    attachClickListener('previous', previousSlide);
    attachClickListener('next', nextSlide);
    attachClickListener('slidePicker', presentSlidePicker);
    attachClickListener('fullScreen', toggleFullScreen);
    attachClickListener('remote', openRemote);

    const deck = document.getElementById('slider');

    if (!deck) {
      return;
    }

    deck.addEventListener('deckDidLoad', async () => {
      await initActions();
    });
  });
};

function attachClickListener(selectorId: string, action: ($event: UIEvent) => Promise<void>) {
  const element: HTMLElement | null = document.getElementById(selectorId);

  if (!element) {
    return;
  }

  element.addEventListener('click', action);
}

function initActions() {
  return new Promise(async (resolve) => {
    const elements: NodeListOf<HTMLElement> = document.querySelectorAll('[slot="actions"]');

    if (elements) {
      Array.from(elements).forEach((element) => {
        element.addEventListener('click', openMenu);
      });
    }

    resolve();
  });
}
