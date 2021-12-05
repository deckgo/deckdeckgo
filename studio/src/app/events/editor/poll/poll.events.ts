export class PollEvents {
  private el: HTMLElement;

  init(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.el = el;

      this.el.addEventListener('slideDidUpdate', this.slideDidUpdate);

      resolve();
    });
  }

  destroy() {
    this.el.removeEventListener('slideDidUpdate', this.slideDidUpdate);
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
