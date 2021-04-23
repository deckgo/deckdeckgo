import { isPapyrus, isScreenshot } from "../utils/utils.deck";
import { isFullscreen } from "@deckdeckgo/utils";

export const initMouse = async () => {
  if (window && 'ResizeObserver' in window) {
    initMainSizeObserver();
  } else {
    await initSizeOldBrowser();
  }
};

const initMainSizeObserver = () => {
  const mainResizeObserver: ResizeObserver = new ResizeObserver(async (_entries) => {
    await attachDetachLaserPointer();
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

  setTimeout(async () => await attachDetachLaserPointer(), 100);
};

const attachDetachLaserPointer = async () => {
  const content: HTMLElement | null = document.querySelector('div.ion-page ion-content');

  if (!content) {
    return;
  }

  const laserPointer: HTMLElement | null = content.querySelector('deckgo-laser-pointer');

  if (isFullscreen()) {
    if (laserPointer) {
      return;
    }

    const newLaserPointer: HTMLElement = document.createElement('deckgo-laser-pointer');
    content.appendChild(newLaserPointer);

    return;
  }

  laserPointer?.parentElement?.removeChild(laserPointer);
};
