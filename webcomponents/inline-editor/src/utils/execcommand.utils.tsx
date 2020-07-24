import {ExecCommandAction} from '../interfaces/interfaces';

import {DeckdeckgoInlineEditorUtils} from './utils';

export async function execCommand(selection: Selection, action: ExecCommandAction, containers: string) {
  if (!document || !selection) {
    return;
  }

  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode || !anchorNode.parentElement) {
    return;
  }

  const style: Node | null = await findStyle(anchorNode.nodeType === 3 ? anchorNode.parentNode : anchorNode, action.style, containers);

  const sameSelection: boolean = style && (style as HTMLElement).innerText === selection.toString();

  if (style && sameSelection) {
    (style as HTMLElement).style[action.style] = action.value;
    return;
  }

  if (anchorNode.nodeType === 1) {
    (anchorNode as HTMLElement).style[action.style] = action.value;
    return;
  }

  if (anchorNode.nodeType === 3) {
    const content = anchorNode.nodeValue;

    const minOffset: number = Math.min(selection.anchorOffset, selection.focusOffset);
    const maxOffset: number = Math.max(selection.anchorOffset, selection.focusOffset);

    const a: Text | null = content.substring(0, minOffset) !== '' ? document.createTextNode(content.substring(0, minOffset)) : null;
    const b: Text | null = content.substring(maxOffset) !== '' ? document.createTextNode(content.substring(maxOffset)) : null;

    const span = document.createElement('span');
    span.style[action.style] = action.value;
    span.innerHTML = selection.toString();

    if (selection.focusNode.nextSibling === null) {
      append(anchorNode, a);
      append(anchorNode, span);
      append(anchorNode, b);
    } else {
      insertBefore(anchorNode, b, selection.focusNode.nextSibling);
      insertBefore(anchorNode, span, selection.focusNode.nextSibling);
      insertBefore(anchorNode, a, selection.focusNode.nextSibling);
    }

    anchorNode.parentElement.removeChild(anchorNode);
  }
}

function append(anchorNode: Node, element: Text | HTMLSpanElement | null) {
  if (element !== null) {
    anchorNode.parentElement.appendChild(element);
  }
}

function insertBefore(anchorNode: Node, element: Text | HTMLSpanElement | null, reference: Node) {
  if (element !== null) {
    anchorNode.parentElement.insertBefore(element, reference);
  }
}

async function findStyle(node: Node, style: string, containers: string): Promise<Node | null> {
  // Just in case
  if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
    return null;
  }

  if (DeckdeckgoInlineEditorUtils.isContainer(containers, node)) {
    return null;
  }

  const hasStyle: boolean =
    (node as HTMLElement).style[style] !== null && (node as HTMLElement).style[style] !== undefined && (node as HTMLElement).style[style] !== '';

  if (hasStyle) {
    return node;
  }

  return await findStyle(node.parentNode, style, containers);
}
