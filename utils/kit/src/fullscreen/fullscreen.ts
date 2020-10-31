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
      },
      {passive: true}
    );

    resolve();
  });
};
