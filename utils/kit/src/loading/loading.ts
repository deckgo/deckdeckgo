import {initEmbedded} from '../embed/embedded';
import { initSize } from "../size/size";
import { isScreenshot } from "../utils/utils.deck";
import { initMouse } from "../mouse/mouse";

export const postLoading = async () => {
  const app = document.querySelector('ion-app');

  if (app) {
    app.classList.remove('loading');
  }

  await initSreenshot();
  await initEmbedMode();
  await initSize();
  await initMouse();
};

async function initSreenshot() {
  if (!window || !window.location) {
    return;
  }

  if (isScreenshot()) {
    const menu: HTMLElement | null = document.querySelector('#fullscreen-menu');

    if (menu) {
      menu.style.display = 'none';
    }

    const previous: HTMLElement | null = document.querySelector('#previous');

    if (previous) {
      previous.style.display = 'none';
    }

    const next: HTMLElement | null = document.querySelector('#next');

    if (next) {
      next.style.display = 'none';
    }

    const header: HTMLElement | null = document.querySelector('ion-header');

    if (header) {
      header.style.display = 'none';
    }

    const deck: HTMLElement | null = document.querySelector('deckgo-deck');
    if (deck) {
      deck.style.setProperty('--pager-display', 'none');

      const actions: HTMLElement | null = deck.querySelector('[slot="actions"]');
      if (actions) {
        actions.style.display = 'none';
      }
    }
  }
}

async function initEmbedMode() {
  initEmbedded();

  if (EMBEDDED) {
    const slidePicker: HTMLElement | null = document.querySelector('#slidePicker');

    if (slidePicker) {
      slidePicker.style.display = 'none';
    }

    const deck: HTMLElement | null = document.querySelector('deckgo-deck');
    if (deck) {
      deck.style.setProperty('--pager-display', 'none');
    }
  }
}
