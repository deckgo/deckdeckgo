import {createEmptyElement, StyloPlugin, StyloPluginCreateParagraphsParams, transformParagraph} from '@deckdeckgo/stylo';

export const hr: StyloPlugin = {
  text: 'separator',
  icon: 'hr',
  createParagraphs: async ({container, paragraph}: StyloPluginCreateParagraphsParams) =>
    transformParagraph({
      elements: [document.createElement('deckgo-hr'), createEmptyElement({nodeName: 'div'})],
      paragraph,
      container
    })
};
