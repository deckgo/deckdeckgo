import {ExecCommandAction} from '../interfaces/interfaces';

export async function execCommand(selection: Selection, action: ExecCommandAction, _containers: string) {
  if (!document || !selection) {
    return;
  }

  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode) {
    return;
  }

  const container: HTMLElement = anchorNode.nodeType === 1 ? (anchorNode as HTMLElement) : anchorNode.parentElement;

  const sameSelection: boolean = container && container.innerText === selection.toString();

  if (sameSelection && container.style[action.style] !== undefined) {
    await updateSelection(container, action);

    return;
  }

  await replaceSelection(action, selection);
}

async function updateSelection(container: HTMLElement, action: ExecCommandAction) {
  container.style[action.style] = action.value;

  await cleanChildren(action, container);
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

  // Clean direct (> *) children with same style
  const children: HTMLElement[] = Array.from(span.children).filter((element: HTMLElement) => {
    return element.style[action.style] !== undefined && element.style[action.style] !== '';
  }) as HTMLElement[];

  if (children && children.length > 0) {
    children.forEach((element: HTMLElement) => {
      element.style[action.style] = '';
    });
  }

  // Direct children (> *) may have children (*) which need to be cleaned too
  const cleanChildrenChildren: Promise<void>[] = Array.from(span.children).map((element: HTMLElement) => {
    return cleanChildren(action, element);
  });

  if (!cleanChildrenChildren || cleanChildrenChildren.length <= 0) {
    return;
  }

  await Promise.all(cleanChildrenChildren);
}

function createSpan(action: ExecCommandAction): HTMLSpanElement {
  const span = document.createElement('span');
  span.style[action.style] = action.value;

  return span;
}
