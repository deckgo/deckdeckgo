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

  if (anchorNode.nodeType !== 3) {
    return;
  }

  const selectionInAnchor: boolean =
    anchorNode.textContent
      .replace(/[\n\r]+/g, '')
      .replace(/ /g, '')
      .indexOf(
        selection
          .toString()
          .replace(/[\n\r]+/g, '')
          .replace(/ /g, '')
      ) > -1;

  if (selectionInAnchor) {
    await extractText(anchorNode, selection, action);
  } else {
    await extractNodes(anchorNode, selection, action);
  }
}

async function extractNodes(anchorNode: Node, selection: Selection, action: ExecCommandAction) {
  const content = anchorNode.parentNode.textContent;

  const {minOffset, maxOffset} = minMaxOffset(selection);

  const a: Text | null =
    minOffset === 0
      ? anchorNode.parentNode.firstChild
        ? document.createTextNode(anchorNode.parentNode.firstChild.textContent)
        : null
      : content.substring(0, maxOffset) !== ''
      ? document.createTextNode(content.substring(0, maxOffset))
      : null;

  const b: Text | null =
    anchorNode.textContent !== null
      ? selection.anchorOffset < selection.focusOffset
        ? document.createTextNode(anchorNode.textContent.substring(minOffset))
        : document.createTextNode(content.substring(maxOffset + selection.toString().length))
      : null;

  const span: HTMLSpanElement = createSpan(action, selection);

  const parent: HTMLElement = anchorNode.parentElement;

  // Remove all children
  parent.textContent = '';

  append(parent, a);
  append(parent, span);
  append(parent, b);
}

async function extractText(anchorNode: Node, selection: Selection, action: ExecCommandAction) {
  const content = anchorNode.nodeValue;

  const {minOffset, maxOffset} = minMaxOffset(selection);

  const a: Text | null = content.substring(0, minOffset) !== '' ? document.createTextNode(content.substring(0, minOffset)) : null;
  const b: Text | null = content.substring(maxOffset) !== '' ? document.createTextNode(content.substring(maxOffset)) : null;

  const span: HTMLSpanElement = createSpan(action, selection);

  if (selection.focusNode.nextSibling === null) {
    append(anchorNode.parentElement, a);
    append(anchorNode.parentElement, span);
    append(anchorNode.parentElement, b);
  } else {
    insertBefore(anchorNode, b, selection.focusNode.nextSibling);
    insertBefore(anchorNode, span, selection.focusNode.nextSibling);
    insertBefore(anchorNode, a, selection.focusNode.nextSibling);
  }

  anchorNode.parentElement.removeChild(anchorNode);
}

function append(parentElement: HTMLElement, element: Text | HTMLSpanElement | null) {
  if (element !== null) {
    parentElement.appendChild(element);
  }
}

function insertBefore(anchorNode: Node, element: Text | HTMLSpanElement | null, reference: Node) {
  if (element !== null) {
    anchorNode.parentElement.insertBefore(element, reference);
  }
}

function createSpan(action: ExecCommandAction, selection: Selection): HTMLSpanElement {
  const span = document.createElement('span');
  span.style[action.style] = action.value;
  span.innerHTML = selection.toString();

  return span;
}

function minMaxOffset(selection: Selection): {minOffset: number; maxOffset: number} {
  const minOffset: number = Math.min(selection.anchorOffset, selection.focusOffset);
  const maxOffset: number = Math.max(selection.anchorOffset, selection.focusOffset);

  return {minOffset, maxOffset};
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
