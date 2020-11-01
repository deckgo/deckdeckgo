export const initFullscreen = () => {
  return new Promise((resolve) => {
    if (!document) {
      resolve();
      return;
    }

    document.addEventListener(
      'mouseInactivity',
      async ($event) => {
        const navigation: HTMLElement | null = document.querySelector('#navigation');

        if ($event && navigation) {
          navigation.style.visibility = ($event as CustomEvent<boolean>).detail ? 'inherit' : 'hidden';
        }

        const previous: HTMLElement | null = document.querySelector('#previous');

        if ($event && previous) {
          previous.style.visibility = ($event as CustomEvent<boolean>).detail ? 'inherit' : 'hidden';
        }

        const next: HTMLElement | null = document.querySelector('#next');

        if ($event && next) {
          next.style.visibility = ($event as CustomEvent<boolean>).detail ? 'inherit' : 'hidden';
        }
      },
      {passive: true}
    );

    resolve();
  });
};
