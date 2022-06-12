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
  'data-gramm',
  'data-gramm_id',
  'data-gramm_editor',
  'data-gr-id'
];

export const dirtyPublishAttributes: string[] = ['paragraph_id', 'data-src', ...dirtyAttributes];

export const cleanNode = ({
  node,
  deep = true,
  attributes = dirtyAttributes
}: {
  node: Node;
  deep?: boolean;
  attributes?: string[];
}): Node | null => {
  if (isTextNode(node)) {
    return node;
  }

  if (isElementNode(node)) {
    const clone: HTMLElement = node.cloneNode(deep) as HTMLElement;
    cleanAttributes({element: clone, attributes});

    const children: NodeListOf<HTMLElement> = clone.querySelectorAll(attributes.map((attr: string) => `[${attr}]`).join(','));
    for (const child of Array.from(children)) {
      cleanAttributes({element: child, attributes});
    }

    return clone;
  }

  return null;
};

const cleanAttributes = ({element, attributes}: {element: HTMLElement; attributes: string[]}) => {
  for (const attr of attributes) {
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
