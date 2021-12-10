import {findParagraph, isParagraph} from './paragraph.utils';
import {isTextNode} from '@deckdeckgo/editor';

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

      return !isParagraph({element: node, container}) && !isTextNode(node) && node?.nodeName.toLowerCase() !== 'br';
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
