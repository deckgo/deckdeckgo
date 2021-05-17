import Prism from 'prismjs';

import {attachHighlightObserver} from './highlight.utils';

export const parseCode = async ({
  refContainer,
  refCode,
  code,
  lineNumbers,
  highlightLines,
  language
}: {
  refContainer: HTMLDivElement | undefined;
  code: string | null | undefined;
  lineNumbers: boolean;
  refCode: HTMLElement;
  highlightLines: string | undefined;
  language: string;
}): Promise<void> => {
  if (!code || code === undefined || code === '') {
    return;
  }

  if (!refContainer) {
    return;
  }

  // clear the container first
  refContainer.children[0].textContent = '';

  // split the code on linebreaks
  const regEx = RegExp(/\n(?!$)/g); //
  const match = code.split(regEx);
  match.forEach((m, idx, array) => {
    // On last element
    if (idx === array.length - 1) {
      attachHighlightObserver({refContainer, refCode, highlightLines});
    }

    let div: HTMLElement = document.createElement('div');
    if (lineNumbers) {
      div.classList.add('deckgo-highlight-code-line-number');
    }

    const highlight: string = Prism.highlight(m, Prism.languages[language], language);

    // If empty, use \u200B as zero width text spacer
    div.innerHTML = highlight && highlight !== '' ? highlight : '\u200B';

    // No text node
    const children: Node[] = Array.from(div.childNodes).map((node: Node) => {
      if (node.nodeName === '#text') {
        const span: HTMLSpanElement = document.createElement('span');
        span.append(node);
        return span;
      }

      return node;
    });

    div.textContent = '';
    div.append(...children);

    refContainer.children[0].appendChild(div);
  });
};
