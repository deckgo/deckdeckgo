export const initFullscreen = () => {
  return new Promise((resolve) => {
    if (!document) {
      resolve();
      return;
    }

    document.addEventListener(
      'mouseInactivity',
      async ($event) => {
        const menu: HTMLElement | null = document.querySelector('#fullscreen-menu');

        if ($event && menu) {
          menu.style.visibility = ($event as CustomEvent<boolean>).detail ? 'inherit' : 'hidden';
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
