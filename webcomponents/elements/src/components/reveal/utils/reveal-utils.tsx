export const findRevealChildren = (el: HTMLElement): Node[] =>
  Array.from(el.childNodes).filter((node: Node) => {
    return node && node.nodeType !== node.TEXT_NODE;
  });
