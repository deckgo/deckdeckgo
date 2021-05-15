import {SlotType} from '../../types/editor/slot-type';
import {FontSize} from '../../types/editor/font-size';

import {h1, h2, normal} from '../../../assets/font-size.json';

export const initFontSize = (element: HTMLElement | undefined): FontSize => {
  if (!element) {
    return FontSize.NORMAL;
  }

  const fontSize: string | undefined = element.style.fontSize;

  if (!fontSize || fontSize === '') {
    return FontSize.NORMAL;
  }

  if (isElementH1(element)) {
    return findFondSize({fonts: h1, fontSize});
  }

  if (isElementH2(element)) {
    return findFondSize({fonts: h2, fontSize});
  }

  return findFondSize({fonts: normal, fontSize});
};

const findFondSize = ({fonts, fontSize}: {fonts: Record<string, string>, fontSize: string}): FontSize => {
  const size: string | undefined = Object.keys(fonts).find((key: string) => fonts[key] === fontSize);
  return FontSize[size?.toUpperCase()] || FontSize.CUSTOM;
}

const applyFontSize = ({fonts, fontSize}: {fonts: Record<string, string>, fontSize: FontSize}): string | undefined => {
  return fonts[fontSize.toLowerCase()];
}

export const toggleFontSize = (element: HTMLElement, fontSize: FontSize | undefined): string | undefined => {
  if (!element || !fontSize) {
    return undefined;
  }

  if (isElementH1(element)) {
    return applyFontSize({fonts: h1, fontSize});
  }

  if (isElementH2(element)) {
    return applyFontSize({fonts: h2, fontSize});
  }

  return applyFontSize({fonts: normal, fontSize});
}

const isElementH1 = (element: HTMLElement | undefined): boolean => {
  return SlotType.H1.toUpperCase() === element?.nodeName.toUpperCase();
};

const isElementH2 = (element: HTMLElement | undefined): boolean => {
  return SlotType.H2.toUpperCase() === element?.nodeName.toUpperCase();
};
