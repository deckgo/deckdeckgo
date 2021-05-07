import {ExecCommandList} from '../interfaces/interfaces';
import {DeckdeckgoInlineEditorUtils} from './utils';

export async function execCommandList(selection: Selection, action: ExecCommandList, containers: string) {
  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode) {
    return;
  }

  const container: HTMLElement | undefined = await DeckdeckgoInlineEditorUtils.findContainer(containers, anchorNode);

  if (!container) {
    return;
  }

  // Did the user select the all list
  if (container.nodeName.toLowerCase() === action.type) {
    await removeList(container);
    return;
  }

  if (!['ol', 'ul', 'dl'].includes(container.nodeName.toLowerCase())) {
    await createList(container, selection, action.type);
    return;
  }

  // Create a brand new list
  await cloneList(container, selection, action.type);
  await removeList(container, false);
}

async function createList(container: HTMLElement, selection: Selection, type: 'ol' | 'ul') {
  const range: Range = selection.getRangeAt(0);

  const fragment: DocumentFragment = range.extractContents();

  const list: HTMLOListElement | HTMLUListElement = document.createElement(type);

  const li: HTMLLIElement = document.createElement('li');
  li.style.cssText = container.style.cssText;
  li.appendChild(fragment);

  list.appendChild(li);

  range.insertNode(list);
  selection.selectAllChildren(list);
}

async function cloneList(container: HTMLElement, selection: Selection, type: 'ol' | 'ul') {
  const list: HTMLOListElement | HTMLUListElement = document.createElement(type);

  list.append(...Array.from(container.childNodes));

  Array.from(container.attributes).forEach((attr: Attr) => list.setAttribute(attr.nodeName, attr.nodeValue));

  container.parentElement.insertBefore(list, container);

  selection.selectAllChildren(list);
}

async function removeList(list: HTMLElement, preserveChildren: boolean = true) {
  if (list.hasChildNodes() && preserveChildren) {
    Array.from(list.childNodes).forEach((child: Node) => {
      if (
        child.hasChildNodes() &&
        child.childNodes.length > 1 &&
        child.firstChild.nodeType !== Node.TEXT_NODE &&
        child.firstChild.nodeType !== Node.COMMENT_NODE
      ) {
        const span: HTMLSpanElement = document.createElement('span');
        span.append(...Array.from(child.childNodes));
        list.parentElement.insertBefore(span, list);
      } else {
        const text: Text = document.createTextNode(child.textContent);
        list.parentElement.insertBefore(text, list);
      }
    });
  }

  list.parentElement.removeChild(list);
}
