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

  // https://stackoverflow.com/a/15229371/5404186
  const range: Range = selection.getRangeAt(0);
  const span: HTMLSpanElement = createSpan(action, selection);
  range.deleteContents();
  range.insertNode(span);
  range.selectNode(span);
  selection.removeAllRanges();
  selection.addRange(range);
}

function createSpan(action: ExecCommandAction, selection: Selection): HTMLSpanElement {
  const span = document.createElement('span');
  span.style[action.style] = action.value;
  span.innerHTML = selection.toString();

  return span;
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
