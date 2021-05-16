import {lazyLoadSelectedImages, lazyLoadSelectedLazyImagesComponent} from '@deckdeckgo/utils';
import {getAllElements} from './element-utils';

export function lazyLoadImages(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const promises: Promise<void>[] = [];

    promises.push(lazyLoadLazyImgTags(el));
    promises.push(lazyLoadLazyImgComponents(el));

    await Promise.all(promises);

    resolve();
  });
}

function lazyLoadLazyImgTags(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const images: HTMLElement[] = getAllElements(el, 'img');

    await lazyLoadSelectedImages(images);

    resolve();
  });
}

function lazyLoadLazyImgComponents(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const images: HTMLElement[] = getAllElements(el, 'deckgo-lazy-img');

    await lazyLoadSelectedLazyImagesComponent(images);

    resolve();
  });
}

export function hideLazyLoadImages(el: HTMLElement): Promise<void> {
  return new Promise<void>((resolve) => {
    let images: HTMLElement[] = getAllElements(el, 'img');

    if (!images) {
      resolve();
    } else {
      images = images.filter((image: HTMLElement) => image.getAttribute('data-src'));

      images.forEach((image: HTMLElement) => {
        image.style.setProperty('visibility', 'hidden');
      });

      resolve();
    }
  });
}
