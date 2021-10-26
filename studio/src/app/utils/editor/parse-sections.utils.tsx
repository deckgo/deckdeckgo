import {h, JSX} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import {Section} from '@deckdeckgo/editor';

import {ParseElementsUtils} from './parse-elements.utils';

export class ParseSectionsUtils {
  static async parseSection({
    section,
    ignoreSectionId = false
  }: {
    section: Section;
    ignoreSectionId?: boolean;
  }): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      let content = undefined;

      // Create a div to parse back to JSX its children
      const div = document.createElement('div');

      if (section.data.content && section.data.content !== undefined) {
        div.innerHTML = section.data.content;
        content = await ParseElementsUtils.parseElements(div, true, false);
      }

      const SectionElement: string = section.data.nodeName;

      const result: JSX.IntrinsicElements = (
        <SectionElement key={uuid()} section_id={ignoreSectionId ? undefined : section.id}>
          {content}
        </SectionElement>
      );

      resolve(result);
    });
  }
}
