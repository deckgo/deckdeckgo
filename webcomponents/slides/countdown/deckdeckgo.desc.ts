import {JsonDocs} from '@stencil/core/internal';
import {JsonDocsComponent, JsonDocsSlot} from '@stencil/core/internal/stencil-public-docs';

import * as fs from 'fs';

interface DeckDeckGoSlot {
  name: string;
  placeholder?: string;
  types?: string[];
}

const parseSlots = (slots: JsonDocsSlot[] | undefined): DeckDeckGoSlot[] | undefined => {
  if (!slots || slots.length <= 0) {
    return undefined;
  }

  return slots.map((slot: JsonDocsSlot) => {
    const docs: string[] | undefined = slot.docs?.split('-');
    const types: string[] | undefined = docs && docs.length >= 2 ? docs[1]?.trim().split(',') : undefined;

    return {
      name: slot.name,
      ...(docs && docs.length >= 1 && docs[0] !== '' && {placeholder: docs[0].trim()}),
      ...(types && types.length > 0 && {types}),
    };
  });
};

export const generateDesc = (docs: JsonDocs) => {
  if (!docs || !docs.components) {
    console.warn('No docs or components provided.');
  }

  const components = docs.components.map((cmp: JsonDocsComponent) => {
    const slots: DeckDeckGoSlot[] | undefined = parseSlots(cmp.slots);

    return {
      tag: cmp.tag,
      props: cmp.props,
      ...(slots && {slots}),
    };
  });

  fs.writeFileSync('./src/components.desc.json', JSON.stringify(components, null, 2));
};
