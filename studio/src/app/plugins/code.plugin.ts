import {createEmptyElement, StyloPlugin, StyloPluginCreateParagraphsParams, transformParagraph} from '@deckdeckgo/stylo';

const createSlottedCode = (): HTMLElement => {
  const code: HTMLElement = document.createElement('code');
  code.setAttribute('slot', 'code');
  return code;
};

export const code: StyloPlugin = {
  text: 'code',
  icon: 'code',
  createParagraphs: async ({container, paragraph}: StyloPluginCreateParagraphsParams) => {
    const code: HTMLElement = document.createElement('deckgo-highlight-code');

    code.setAttribute('editable', 'true');
    code.append(createSlottedCode());

    transformParagraph({
      elements: [code, createEmptyElement({nodeName: 'div'})],
      paragraph,
      container
    });
  }
};
