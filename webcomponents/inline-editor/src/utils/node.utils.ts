import {DeckdeckgoInlineEditorUtils} from './utils';

export const findStyleNode = (node: Node, style: string, containers: string): Node | null => {
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

  return findStyleNode(node.parentNode, style, containers);
};
