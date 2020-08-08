import {ExecCommandList} from '../interfaces/interfaces';

export async function execCommandList(selection: Selection, action: ExecCommandList) {
  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode) {
    return;
  }

  const container: HTMLElement =
    anchorNode.nodeType !== Node.TEXT_NODE && anchorNode.nodeType !== Node.COMMENT_NODE ? (anchorNode as HTMLElement) : anchorNode.parentElement;

  const range: Range = selection.getRangeAt(0);

  // Did the user select the all list
  if (range.commonAncestorContainer && range.commonAncestorContainer.nodeName.toLowerCase() === action.type) {
    await removeList(range);

    return;
  }

  // Did the user select an element of the list
  if (container.nodeName.toLowerCase() === 'li') {
    await removeItem(container, range, selection, action.type);

    return;
  }

  // Create a brand new list
  await createList(container, range, selection, action.type);
}

async function createList(container: HTMLElement, range: Range, selection: Selection, type: 'ol' | 'ul') {
  const fragment: DocumentFragment = range.extractContents();

  const list: HTMLOListElement | HTMLUListElement = document.createElement(type);

  const li: HTMLLIElement = document.createElement('li');
  li.style.cssText = container.style.cssText;
  li.appendChild(fragment);

  list.appendChild(li);

  range.insertNode(list);
  selection.selectAllChildren(list);
}

async function removeList(range: Range) {
  const list: Node = range.commonAncestorContainer;
  if (list.hasChildNodes()) {
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

async function removeItem(container: HTMLElement, range: Range, selection: Selection, type: 'ol' | 'ul') {
  movePreviousSiblings(container, type);
  moveNextSiblings(container, type);

  // Finally convert selected item to not be part of the list anymore
  const span: HTMLSpanElement = document.createElement('span');
  span.style.cssText = container.style.cssText;

  const fragment: DocumentFragment = range.extractContents();
  span.appendChild(fragment);

  container.parentElement.parentElement.insertBefore(
    span,
    container.parentElement.nextElementSibling ? container.parentElement.nextElementSibling : container.parentElement.parentElement.lastChild
  );
  selection.selectAllChildren(container);

  const list = container.parentElement;
  list.removeChild(container);

  if (!list.hasChildNodes()) {
    list.parentElement.removeChild(list);
  }
}

function movePreviousSiblings(container: HTMLElement, type: 'ol' | 'ul') {
  if (container.previousElementSibling && container.previousElementSibling.nodeName.toLowerCase() === 'li') {
    const list: HTMLElement | null = moveSibling(container.previousElementSibling, true, type);

    if (list) {
      container.parentElement.parentElement.insertBefore(list, container.parentElement);
    }
  }
}

function moveNextSiblings(container: HTMLElement, type: 'ol' | 'ul') {
  if (container.nextElementSibling && container.nextElementSibling.nodeName.toLowerCase() === 'li') {
    const list: HTMLElement | null = moveSibling(container.nextElementSibling, false, type);

    if (list) {
      container.parentElement.nextSibling
        ? container.parentElement.parentElement.insertBefore(list, container.parentElement.nextSibling)
        : container.parentElement.parentElement.appendChild(list);
    }
  }
}

function moveSibling(sibling: Element | null, previous: boolean, type: 'ol' | 'ul'): HTMLOListElement | HTMLUListElement | null {
  if (!sibling || sibling.nodeName.toLowerCase() !== 'li') {
    return null;
  }

  const children = [];

  while (sibling && sibling.nodeName.toLowerCase() === 'li') {
    children.push(sibling);

    sibling = previous ? sibling.previousElementSibling : sibling.nextElementSibling;
  }

  if (!children || children.length <= 0) {
    return null;
  }

  const list: HTMLOListElement | HTMLUListElement = document.createElement(type);

  if (previous) {
    list.append(...children.reverse());
  } else {
    list.append(...children);
  }

  return list;
}
