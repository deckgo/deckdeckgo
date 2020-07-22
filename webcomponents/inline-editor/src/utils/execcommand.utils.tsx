import {ExecCommandAction} from '../interfaces/interfaces';

import {DeckdeckgoInlineEditorUtils} from './utils';

export async function execCommand(selection: Selection, action: ExecCommandAction, containers: string) {
  if (!document) {
    return;
  }

  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode || !anchorNode.parentElement) {
    return;
  }

  // TODO: Does not work yet
  const style: Node | null = await findStyle(anchorNode.nodeType === 3 ? anchorNode.parentNode : anchorNode, action, containers);

  if (style) {
    (style as HTMLElement).style[action.style] = action.value;
    return;
  }

  if (anchorNode.nodeType === 1) {
    (anchorNode as HTMLElement).style[action.style] = action.value;
    return;
  }

  if (anchorNode.nodeType === 3) {
    const content = anchorNode.nodeValue;

    const a = document.createTextNode(content.substring(0, selection.anchorOffset));

    // TODO: last char
    const b = document.createTextNode(content.substring(selection.focusOffset));

    const span = document.createElement('span');
    span.style[action.style] = action.value;
    span.innerHTML = selection.toString();

    if (selection.focusNode.nextSibling === null) {
      anchorNode.parentElement.appendChild(a);
      anchorNode.parentElement.appendChild(span);
      anchorNode.parentElement.appendChild(b);
    } else {
      anchorNode.parentElement.insertBefore(b, selection.focusNode.nextSibling);
      anchorNode.parentElement.insertBefore(span, selection.focusNode.nextSibling);
      anchorNode.parentElement.insertBefore(a, selection.focusNode.nextSibling);
    }

    anchorNode.parentElement.removeChild(anchorNode);
  }
}

async function findStyle(node: Node, action: ExecCommandAction, containers: string): Promise<Node | null> {
  // Just in case
  if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
    return null;
  }

  const hasStyle: boolean = (node as HTMLElement).style[action.style] !== null;
  const sameStyleValue: boolean = (node as HTMLElement).style[action.style] === action.value;

  if (DeckdeckgoInlineEditorUtils.isContainer(containers, node) || hasStyle) {
    return sameStyleValue ? node : null;
  }

  await findStyle(node.parentNode, action, containers);
}
