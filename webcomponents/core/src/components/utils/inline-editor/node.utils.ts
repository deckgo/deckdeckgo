export const getAnchorElement = (selection: Selection | undefined): HTMLElement | undefined => {
  const anchorNode: Node | undefined = selection?.anchorNode;

  if (!anchorNode) {
    return undefined;
  }

  return anchorNode.nodeType !== Node.TEXT_NODE && anchorNode.nodeType !== Node.COMMENT_NODE
    ? (anchorNode as HTMLElement)
    : anchorNode.parentElement;
};
