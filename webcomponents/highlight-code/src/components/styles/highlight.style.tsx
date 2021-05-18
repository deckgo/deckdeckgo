import {FunctionalComponent, h} from '@stencil/core';

export const HighlightStyle: FunctionalComponent<{start?: number; end?: number}> = ({start, end}) => {
  const selectorGroup: string =
    start !== undefined && end !== undefined
      ? `code.highlight > :nth-child(n+${start + 1}):nth-child(-n+${end + 1}) *`
      : 'div.container code.highlight > div.highlight *';
  const selectorLineNumbers: string =
    start !== undefined && end !== undefined
      ? `code.highlight > div.line-number:nth-child(n+${start + 1}):nth-child(-n+${end + 1}):before`
      : 'div.container code.highlight > div.highlight:before';

  return (
    <style>{`
      ${selectorGroup} {
        background: var(--deckgo-highlight-code-line-background);
        border-top: var(--deckgo-highlight-code-line-border-top);
        border-bottom: var(--deckgo-highlight-code-line-border-bottom);
        font-weight: var(--deckgo-highlight-code-line-font-weight);
        opacity: var(--deckgo-highlight-code-line-opacity, 1);
      }

      ${selectorLineNumbers} {
        color: var(--deckgo-highlight-code-line-numbers-color, var(--deckgo-highlight-code-token-comment, #6272a4));
      }
    `}</style>
  );
};
