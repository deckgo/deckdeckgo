export const dirtyAttributes: string[] = [
  'id',
  'hydrated',
  'contenteditable',
  'editable',
  'spellcheck',
  'highlighted',
  'custom-loader',
  'class',
  'placeholder',
  'data-gramm'
];

export const cleanNode = ({node, deep = true}: {node: Node; deep?: boolean}): Node | null => {
  if (isTextNode(node)) {
    return node;
  }

  if (isElementNode(node)) {
    const clone: HTMLElement = node.cloneNode(deep) as HTMLElement;
    cleanAttributes(clone);

    const children: NodeListOf<HTMLElement> = clone.querySelectorAll(dirtyAttributes.map((attr: string) => `[${attr}]`).join(','));
    for (const child of Array.from(children)) {
      cleanAttributes(child);
    }

    return clone;
  }

  return null;
};

const cleanAttributes = (element: HTMLElement) => {
  for (const attr of dirtyAttributes) {
    element.removeAttribute(attr);
  }
};

export const isTextNode = (element: Node | undefined): boolean => {
  return element?.nodeType === Node.TEXT_NODE || element?.nodeType === Node.COMMENT_NODE;
};

export const isElementNode = (element: Node | undefined): boolean => {
  return element?.nodeType === Node.ELEMENT_NODE;
};

export const elementIndex = (element: HTMLElement): number => {
  return Array.from(element.parentNode?.children || []).indexOf(element);
};

export const nodeIndex = (node: Node): number => {
  return Array.from(node.parentNode?.childNodes || []).indexOf(node as ChildNode);
};
