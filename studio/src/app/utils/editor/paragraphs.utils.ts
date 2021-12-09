import {findParagraph} from './paragraph.utils';

export const mapAddParagraphs = ({
  mutations,
  container
}: {
  mutations: MutationRecord[] | undefined;
  container: HTMLElement;
}): HTMLElement[] => {
  if (!mutations || mutations.length <= 0) {
    return [];
  }

  const addedNodes: Node[] = mutations.reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

  return findParagraphs({nodes: addedNodes, container});
};

const findParagraphs = ({nodes, container}: {nodes: Node[]; container: HTMLElement}): HTMLElement[] => [
  ...new Set(
    nodes
      .map((node: Node) => findParagraph({element: node, container}))
      .filter(
        (paragraph: Node | undefined) => paragraph?.nodeType !== Node.TEXT_NODE && paragraph?.nodeType !== Node.COMMENT_NODE
      ) as HTMLElement[]
  )
];
