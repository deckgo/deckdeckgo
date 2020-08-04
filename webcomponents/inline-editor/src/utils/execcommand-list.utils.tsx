import {ExecCommandList} from '../interfaces/interfaces';

// TODO Ol or Ul

export async function execCommandList(selection: Selection, _action: ExecCommandList, _containers: string) {
  const anchorNode: Node = selection.anchorNode;

  if (!anchorNode) {
    return;
  }

  const container: HTMLElement = anchorNode.nodeType !== Node.TEXT_NODE ? (anchorNode as HTMLElement) : anchorNode.parentElement;

  const range: Range = selection.getRangeAt(0);

  // Did the user select the all list
  if (range.commonAncestorContainer && range.commonAncestorContainer.nodeName.toLowerCase() === 'ol') {
    await removeList(range);

    return;
  }

  // Did the user select an element of the list
  if (container.nodeName.toLowerCase() === 'li') {
    await removeItem(container, range, selection);

    return;
  }

  // Create a brand new list
  await createList(range, selection);
}

async function createList(range: Range, selection: Selection) {
  const fragment: DocumentFragment = range.extractContents();

  const ol = document.createElement('ol');
  const li = document.createElement('li');
  li.appendChild(fragment);

  ol.appendChild(li);

  range.insertNode(ol);
  selection.selectAllChildren(ol);
}

async function removeList(range: Range) {
  const list: Node = range.commonAncestorContainer;
  if (list.hasChildNodes()) {
    Array.from(list.childNodes).forEach((child: Node) => {
      if (child.hasChildNodes() && child.childNodes.length > 1 && child.firstChild.nodeType !== Node.TEXT_NODE) {
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

async function removeItem(container: HTMLElement, range: Range, selection: Selection) {
  movePreviousSiblings(container);
  moveNextSiblings(container);

  // Finally convert selected item to not be part of the list anymore
  const fragment: DocumentFragment = range.extractContents();

  container.parentElement.parentElement.insertBefore(
    fragment,
    container.parentElement.nextElementSibling ? container.parentElement.nextElementSibling : container.parentElement.parentElement.lastChild
  );
  selection.selectAllChildren(container);

  const list = container.parentElement;
  list.removeChild(container);

  if (!list.hasChildNodes()) {
    list.parentElement.removeChild(list);
  }
}

function movePreviousSiblings(container: HTMLElement) {
  if (container.previousElementSibling && container.previousElementSibling.nodeName.toLowerCase() === 'li') {
    const list: HTMLElement | null = moveSibling(container.previousElementSibling, true);

    if (list) {
      container.parentElement.parentElement.insertBefore(list, container.parentElement);
    }
  }
}

function moveNextSiblings(container: HTMLElement) {
  if (container.nextElementSibling && container.nextElementSibling.nodeName.toLowerCase() === 'li') {
    const list: HTMLElement | null = moveSibling(container.nextElementSibling, false);

    if (list) {
      container.parentElement.nextSibling
        ? container.parentElement.parentElement.insertBefore(list, container.parentElement.nextSibling)
        : container.parentElement.parentElement.appendChild(list);
    }
  }
}

function moveSibling(sibling: Element | null, previous: boolean): HTMLElement | null {
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

  const list: HTMLElement = document.createElement('ol');

  if (previous) {
    list.append(...children.reverse());
  } else {
    list.append(...children);
  }

  return list;
}
