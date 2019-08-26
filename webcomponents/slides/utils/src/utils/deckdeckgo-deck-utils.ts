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

export function lazyLoadSelectedImages(images: HTMLElement[]): Promise<void> {
    return new Promise<void>((resolve) => {
        if (!images) {
            resolve();
            return;
        }

        images.forEach((image: HTMLElement) => {
            if (image.hasAttribute('data-src')) {
                image.setAttribute('src', `${image.getAttribute('data-src')}`);
                image.removeAttribute('data-src');

                // If image is part of a reveal group, let it be revealed with the reveal feature
                if (!image.classList.contains('deckgo-reveal')) {
                    image.style.setProperty('visibility', 'inherit');
                }
            }

            // Furthermore to lazy loading, we set pointer-events to none. Doing so we prevent images of being dragged.
            image.style.setProperty('pointer-events', 'none');
        });

        resolve();
    });
}

export function lazyLoadSelectedLazyImagesComponent(components: HTMLElement[]): Promise<void> {
    return new Promise<void>((resolve) => {
        if (!components) {
            resolve();
            return;
        }

        components.forEach(async (component: HTMLElement) => {
            await (component as any).lazyLoad();
        });

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
