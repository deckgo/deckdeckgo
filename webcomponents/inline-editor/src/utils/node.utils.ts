import {DeckdeckgoInlineEditorUtils} from './utils';

export const findStyleNode = async (node: Node, style: string, containers: string): Promise<Node | null> => {
  // Just in case
  if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
    return null;
  }

  if (!node.parentNode) {
    return null;
  }

  if (DeckdeckgoInlineEditorUtils.isContainer(containers, node)) {
    return null;
  }

  const hasStyle: boolean =
    (node as HTMLElement).style[style] !== null &&
    (node as HTMLElement).style[style] !== undefined &&
    (node as HTMLElement).style[style] !== '';

  if (hasStyle) {
    return node;
  }

  return await findStyleNode(node.parentNode, style, containers);
};

export const getAnchorNode = (selection: Selection | undefined): HTMLElement | undefined => {
  const anchorNode: Node = selection?.anchorNode;

  if (!anchorNode) {
    return undefined;
  }

  return anchorNode.nodeType !== Node.TEXT_NODE && anchorNode.nodeType !== Node.COMMENT_NODE
    ? (anchorNode as HTMLElement)
    : anchorNode.parentElement;
};
