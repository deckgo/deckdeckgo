export const updateSlidesQRCode = (publishUrl: string) => {
  const slides: NodeListOf<HTMLElement> = document.querySelectorAll('deckgo-slide-qrcode');

  if (!slides || slides.length <= 0) {
    return;
  }

  for (const slide of Array.from(slides)) {
    if (!slide.hasAttribute('custom-qrcode')) {
      slide.setAttribute('content', publishUrl);
    }
  }
};
