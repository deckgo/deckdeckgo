export class PollEventsHandler {
  private el: HTMLElement;

  init(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.el = el;

      this.el.addEventListener('slideDidUpdate', this.slideDidUpdate, false);

      resolve();
    });
  }

  destroy() {
    this.el.removeEventListener('slideDidUpdate', this.slideDidUpdate, true);
  }

  private slideDidUpdate = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    const slide: HTMLElement = $event.detail;

    if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-poll') {
      return;
    }

    if (typeof (slide as any).update === 'function') {
      await (slide as any).update();
    }
  };
}
