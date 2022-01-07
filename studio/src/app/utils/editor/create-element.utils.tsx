import {h, JSX} from '@stencil/core';

import {SlotType} from '../../types/editor/slot-type';

export const createElement = ({slotType, slotName}: {slotType: SlotType; slotName?: string}): JSX.IntrinsicElements => {
  const Element = slotType.toString();

  return (
    <Element slot={slotName}>
      {slotType === SlotType.OL || slotType === SlotType.UL ? (
        <li>{'\u200B'}</li>
      ) : slotType === SlotType.CODE ? (
        <code slot="code"></code>
      ) : slotType === SlotType.MATH ? (
        <code slot="math"></code>
      ) : slotType === SlotType.WORD_CLOUD ? (
        <code slot="words"></code>
      ) : slotType === SlotType.MARKDOWN ? (
        <div slot="markdown"></div>
      ) : undefined}
    </Element>
  );
};

export const createHTMLElement = ({slotType, slotName}: {slotType: SlotType; slotName?: string}): HTMLElement => {
  const element: HTMLElement = document.createElement(slotType);

  if (slotType === SlotType.OL || slotType === SlotType.UL) {
    element.appendChild(createHTMLListItem());
  } else if ([SlotType.CODE, SlotType.MATH, SlotType.WORD_CLOUD, SlotType.MARKDOWN].includes(slotType)) {
    element.appendChild(createHTMLSlotCode(slotType));
  } else if (SlotType.HR !== slotType) {
    // HR are not editable
    element.innerHTML = '\u200B';
  }

  if (slotName) {
    element.setAttribute('slot', slotName);
  }

  return element;
};

const createHTMLListItem = () => {
  const item: HTMLLIElement = document.createElement('li');
  item.innerHTML = '\u200B';
  return item;
};

const createHTMLSlotCode = (slotType: SlotType) => {
  const code: HTMLElement = document.createElement('code');
  code.setAttribute(
    'slot',
    slotType === SlotType.MATH ? 'math' : slotType === SlotType.WORD_CLOUD ? 'words' : slotType === SlotType.MARKDOWN ? 'markdown' : 'code'
  );
  return code;
};
