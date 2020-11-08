export function getAllElements(el: HTMLElement, tag: string): HTMLElement[] {
  const allSlottedElements: NodeListOf<HTMLElement> = el.querySelectorAll(tag);
  const allShadowsElements: NodeListOf<HTMLElement> | [] = el.shadowRoot ? el.shadowRoot.querySelectorAll(tag) : [];

  return Array.from(allSlottedElements).concat(Array.from(allShadowsElements));
}
