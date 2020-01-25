export function initHowTo(el: HTMLElement, pollKey: string): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const howToSlotElement: HTMLElement = el.querySelector(":scope > [slot='how-to']");

    if (!howToSlotElement) {
      resolve();
      return;
    }

    const container: HTMLElement = el.shadowRoot.querySelector('div.deckgo-slide-poll-qrcode');

    if (!container) {
      resolve();
      return;
    }

    const howTo: HTMLElement = container.querySelector('.how-to');

    if (howTo) {
      container.removeChild(howTo);
    }

    const element: HTMLElement = await cloneHowTo(howToSlotElement, pollKey);
    container.appendChild(element);

    resolve();
  });
}

function cloneHowTo(howToSlotElement: HTMLElement, pollKey: string): Promise<HTMLElement> {
  return new Promise<HTMLElement>((resolve) => {
    const element: HTMLElement = howToSlotElement.cloneNode(true) as HTMLElement;
    element.removeAttribute('slot');
    element.classList.add('how-to');

    if (!element.innerHTML || element.innerHTML === undefined || element.innerHTML.indexOf('{0}') === -1) {
      resolve(element);
      return;
    }

    const replaceWith: string = pollKey ? pollKey.toString().replace(/\B(?=(\d{2})+(?!\d))/g, ' ') : '{0}';

    element.innerHTML = element.innerHTML.replace(/\{0\}/g, replaceWith);

    resolve(element);
  });
}
