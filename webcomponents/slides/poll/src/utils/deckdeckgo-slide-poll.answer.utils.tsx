export function initAnswerSlotsList(el: HTMLElement): Promise<string[] | undefined> {
  return new Promise<string[] | undefined>((resolve) => {
    const slots: NodeListOf<HTMLElement> = el.querySelectorAll(":scope > [slot^='answer']");

    if (!slots || slots.length <= 0) {
      resolve(undefined);
      return;
    }

    const answers: string[] = Array.from(slots).map((slot: HTMLElement) => {
      return slot.getAttribute('slot');
    });

    resolve(answers);
  });
}
