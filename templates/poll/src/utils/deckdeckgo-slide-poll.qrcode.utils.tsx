export async function generateQRCode(el: HTMLElement) {
  const qrCodeElement: HTMLElement = el.shadowRoot.querySelector('deckgo-qrcode');

  if (qrCodeElement && typeof (qrCodeElement as any).generate === 'function') {
    await (qrCodeElement as any).generate();
  }
}

export function initQRCodeSize(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const container: HTMLElement = el.shadowRoot.querySelector('div.deckgo-slide-poll-qrcode');

    if (container) {
      const width: number = container.clientWidth;
      const height: number = container.clientHeight;

      const qrCode: HTMLElement = container.querySelector('deckgo-qrcode');

      const slotHowToHeight: number = await getSlotHowToHeight(el);

      const paddingBottom: number = await getSlideContainerPaddingBottom(el);

      if (qrCode && width > 0 && height > 0) {
        qrCode.style.setProperty(
          '--deckgo-qrcode-size',
          width > height
            ? `calc(${height}px - ${slotHowToHeight}px - ${paddingBottom}px)`
            : `calc(${width}px - ${slotHowToHeight}px - ${paddingBottom}px)`
        );
      }
    }

    resolve();
  });
}

function getSlotHowToHeight(el: HTMLElement): Promise<number> {
  return new Promise<number>((resolve) => {
    const howToElement: HTMLElement = el.querySelector(":scope > [slot='how-to']");

    if (!howToElement || !window) {
      resolve(0);
      return;
    }

    const css: CSSStyleDeclaration = window.getComputedStyle(howToElement);

    const marginTop: number = css ? parseInt(css.marginTop) : 0;
    const marginBottom: number = css ? parseInt(css.marginBottom) : 0;
    const paddingTop: number = css ? parseInt(css.paddingTop) : 0;
    const paddingBottom: number = css ? parseInt(css.paddingBottom) : 0;

    resolve(howToElement.offsetHeight + marginBottom + marginTop + paddingBottom + paddingTop);
  });
}

function getSlideContainerPaddingBottom(el: HTMLElement): Promise<number> {
  return new Promise<number>((resolve) => {
    const slideContainer: HTMLElement = el.shadowRoot.querySelector('div.deckgo-slide');

    if (!slideContainer || !window) {
      resolve(0);
      return;
    }

    const css: CSSStyleDeclaration = window.getComputedStyle(slideContainer);

    resolve(css ? parseInt(css.paddingBottom) : 0);
  });
}
