export const addAnchors = async ({
  refContainer,
  anchor,
  hideAnchor
}: {
  refContainer: HTMLDivElement;
  anchor: string;
  hideAnchor: boolean;
}) => {
  const elements: NodeListOf<HTMLElement> = refContainer.querySelectorAll('span.comment');

  if (!elements) {
    return;
  }

  const elementsArray: HTMLElement[] = Array.from(elements);

  const anchors: HTMLElement[] = elementsArray.filter((element: HTMLElement) => {
    return hasLineAnchor({line: element.innerHTML, anchor});
  });

  if (anchors) {
    anchors.forEach((anchor: HTMLElement) => {
      anchor.classList.add('deckgo-highlight-code-anchor');

      if (hideAnchor) {
        anchor.classList.add('deckgo-highlight-code-anchor-hidden');
      }
    });
  }
};

const hasLineAnchor = ({anchor, line}: {line: string; anchor: string}): boolean => {
  return line && anchor && line.indexOf('@Prop') === -1 && line.split(' ').join('').indexOf(anchor.split(' ').join('')) > -1;
};
