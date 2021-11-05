import {h, JSX} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import {Paragraph} from '@deckdeckgo/editor';

import {ParseElementsUtils} from './parse-elements.utils';

export class ParseParagraphsUtils {
  static async parseParagraph({
    paragraph,
    ignoreParagraphId = false
  }: {
    paragraph: Paragraph;
    ignoreParagraphId?: boolean;
  }): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      let content = undefined;

      // Create a div to parse back to JSX its children
      const div = document.createElement('div');

      if (paragraph.data.children?.length > 0) {
        div.innerHTML = paragraph.data.children.join('');
        content = await ParseElementsUtils.parseElements(div, true, false);
      }

      const ParagraphElement: string = paragraph.data.nodeName;

      const result: JSX.IntrinsicElements = (
        <ParagraphElement key={uuid()} paragraph_id={ignoreParagraphId ? undefined : paragraph.id}>
          {content}
        </ParagraphElement>
      );

      resolve(result);
    });
  }
}
