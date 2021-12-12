import {getSelection} from '@deckdeckgo/utils';

import {findParagraph, isParagraph} from './paragraph.utils';
import {NodeUtils} from './node.utils';

export interface RemovedParagraph {
  paragraph: HTMLElement;
  previousSibling: Node;
}

export const findAddedParagraphs = ({
  mutations,
  container
}: {
  mutations: MutationRecord[] | undefined;
  container: HTMLElement;
}): HTMLElement[] => {
  if (!mutations || mutations.length <= 0) {
    return [];
  }

  const addedNodes: Node[] = mutations
    .filter(({addedNodes}: MutationRecord) => addedNodes?.length > 0)
    .reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

  return filterAddedParagraphs({nodes: addedNodes, container});
};

export const findAddedNodesParagraphs = ({
  mutations,
  container
}: {
  mutations: MutationRecord[] | undefined;
  container: HTMLElement;
}): MutationRecord[] => {
  return mutations
    .filter(({addedNodes}: MutationRecord) => addedNodes?.length > 0)
    .filter(({addedNodes}: MutationRecord) => {
      const node: Node = addedNodes[0];

      return !isParagraph({element: node, container}) && node?.nodeName.toLowerCase() !== 'br';
    });
};

export const findRemovedNodesParagraphs = ({
  mutations,
  container
}: {
  mutations: MutationRecord[] | undefined;
  container: HTMLElement;
}): MutationRecord[] => {
  return mutations
    .filter(({removedNodes}: MutationRecord) => removedNodes?.length > 0)
    .filter(({removedNodes}: MutationRecord) => {
      const node: Node = removedNodes[0];

      return !isParagraph({element: node, container}) && node?.nodeName.toLowerCase() !== 'br';
    });
};

export const findRemovedParagraphs = ({mutations}: {mutations: MutationRecord[] | undefined}): RemovedParagraph[] => {
  if (!mutations || mutations.length <= 0) {
    return [];
  }

  return mutations
    .filter(({removedNodes}: MutationRecord) => removedNodes?.length > 0)
    .reduce((acc: RemovedParagraph[], {removedNodes, previousSibling}: MutationRecord) => {
      const paragraphs: Node[] = filterRemovedParagraphs({nodes: Array.from(removedNodes)});

      return [...acc, ...paragraphs.map((paragraph: HTMLElement) => ({paragraph, previousSibling}))];
    }, []);
};

export const findUpdatedParagraphs = ({
  mutations,
  container
}: {
  mutations: MutationRecord[] | undefined;
  container: HTMLElement;
}): HTMLElement[] => {
  if (!mutations || mutations.length <= 0) {
    return [];
  }

  const nodes: Node[] = mutations.reduce((acc: Node[], {target}: MutationRecord) => [...acc, target], []);

  return [
    ...new Set(
      nodes
        .map((node: Node) => findParagraph({element: node, container}))
        .filter(
          (paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE
        ) as HTMLElement[]
    )
  ];
};

const filterAddedParagraphs = ({nodes, container}: {nodes: Node[]; container: HTMLElement}): HTMLElement[] => {
  return nodes
    .filter((node: Node) => isParagraph({element: node, container}))
    .filter(
      (paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE
    ) as HTMLElement[];
};

const filterRemovedParagraphs = ({nodes}: {nodes: Node[]}): HTMLElement[] => {
  return nodes
    .filter((paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE)
    .filter((node: Node) => (node as HTMLElement).hasAttribute('paragraph_id')) as HTMLElement[];
};

export const findSelectionParagraphs = ({container}: {container: HTMLElement}): HTMLElement[] => {
  const selection: Selection | null = getSelection();
  const range: Range | undefined = selection?.rangeCount > 0 ? selection?.getRangeAt(0) : undefined;

  if (!range) {
    return [];
  }

  const start: HTMLElement | undefined = NodeUtils.toHTMLElement(findParagraph({element: range.startContainer, container}));
  const end: HTMLElement | undefined = NodeUtils.toHTMLElement(findParagraph({element: range.endContainer, container}));

  if (!end || start?.isSameNode(end)) {
    return start ? [start] : [];
  }

  if (start.nextElementSibling.isSameNode(end)) {
    return [start, end];
  }

  const nodes: HTMLElement[] = [];

  let next: Element | null = start.nextElementSibling;
  while (next !== null && !next.isSameNode(end)) {
    nodes.push(NodeUtils.toHTMLElement(next));
    next = next.nextElementSibling;
  }

  return [start, ...nodes, end];
};
