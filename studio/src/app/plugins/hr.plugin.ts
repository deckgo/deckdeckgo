import i18n from '../stores/i18n.store';
import {createEmptyElement, StyloPlugin, StyloPluginCreateParagraphsParams, transformParagraph} from '@deckdeckgo/stylo';

export const hr: StyloPlugin = {
  text: i18n.state.editor.separator,
  icon: 'hr',
  createParagraphs: async ({container, paragraph}: StyloPluginCreateParagraphsParams) =>
    transformParagraph({
      elements: [document.createElement('deckgo-hr'), createEmptyElement({nodeName: 'div'})],
      paragraph,
      container
    })
};
