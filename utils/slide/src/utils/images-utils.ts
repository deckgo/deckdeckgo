import {lazyLoadSelectedImages, lazyLoadSelectedLazyImagesComponent} from '@deckdeckgo/utils';

export function lazyLoadImages(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
        const promises = [];

        promises.push(lazyLoadLazyImgTags(el));
        promises.push(lazyLoadLazyImgComponents(el));

        await Promise.all(promises);

        resolve();
    });
}

function lazyLoadLazyImgTags(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
        const images: HTMLElement[] = getAllImages(el, 'img');

        await lazyLoadSelectedImages(images);

        resolve();
    });
}

function lazyLoadLazyImgComponents(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
        const images: HTMLElement[] = getAllImages(el, 'deckgo-lazy-img');

        await lazyLoadSelectedLazyImagesComponent(images);

        resolve();
    });
}

export function hideLazyLoadImages(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
        let images: HTMLElement[] = getAllImages(el, 'img');

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

function getAllImages(el: HTMLElement, tag: string): HTMLElement[] {
    const allSlotedImages: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] ' + tag);
    const allShadowImages: NodeListOf<HTMLElement> | [] = el.shadowRoot ? el.shadowRoot.querySelectorAll(tag) : [];

    return Array.from(allSlotedImages).concat(Array.from(allShadowImages));
}
