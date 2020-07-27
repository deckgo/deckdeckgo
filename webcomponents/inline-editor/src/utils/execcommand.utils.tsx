import {ExecCommandAction} from '../interfaces/interfaces';

export async function execCommand(selection: Selection, action: ExecCommandAction, _containers: string) {
  if (!document || !selection) {
    return;
  }

  await replaceSelection(action, selection);
}

async function replaceSelection(action: ExecCommandAction, selection: Selection) {
  const range: Range = selection.getRangeAt(0);

  const fragment: DocumentFragment = range.extractContents();

  const span: HTMLSpanElement = createSpan(action);
  span.appendChild(fragment);

  await cleanChildren(action, span);

  range.insertNode(span);
  selection.selectAllChildren(span);
}

async function cleanChildren(action: ExecCommandAction, span: HTMLSpanElement) {
  if (!span.hasChildNodes()) {
    return;
  }

  const children: HTMLElement[] = Array.from(span.children).filter((element: HTMLElement) => {
    return element.style[action.style] !== undefined && element.style[action.style] !== '';
  }) as HTMLElement[];

  if (!children || children.length <= 0) {
    return;
  }

  children.forEach((element: HTMLElement) => {
    element.style[action.style] = '';
  });
}

function createSpan(action: ExecCommandAction): HTMLSpanElement {
  const span = document.createElement('span');
  span.style[action.style] = action.value;

  return span;
}
