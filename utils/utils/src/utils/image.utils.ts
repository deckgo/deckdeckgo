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
