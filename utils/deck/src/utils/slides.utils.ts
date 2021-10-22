import {DeckdeckgoSlideDefinition, DeckdeckgoAttributeDefinition} from '@deckdeckgo/types';

export const findSlidesTitle = async (deckSelector: string = 'deckgo-deck'): Promise<string[]> => {
  const slides: NodeListOf<HTMLElement> = document.querySelectorAll(`${deckSelector} > *`);

  if (!slides) {
    return [];
  }

  return Array.from(slides)
    .filter((slide: HTMLElement) => isSlide(slide))
    .map((slide: HTMLElement, index: number) => {
      const title: HTMLElement | null = slide.querySelector('[slot="title"],[slot="question"]');

      if (title && title.textContent && title.textContent !== '') {
        return title.textContent;
      } else {
        const start: HTMLElement | null = slide.querySelector('[slot="start"],[slot="header"]');

        if (start && start.textContent && start.textContent !== '') {
          return start.textContent;
        } else {
          const end: HTMLElement | null = slide.querySelector('[slot="end"],[slot="footer"]');

          if (end && end.textContent && end.textContent !== '') {
            return end.textContent;
          } else {
            return `Slide #${index}`;
          }
        }
      }
    });
};

export function getSlideDefinition(slide: HTMLElement): Promise<DeckdeckgoSlideDefinition | null> {
  return new Promise<DeckdeckgoSlideDefinition | null>(async (resolve) => {
    if (!slide) {
      resolve(null);
      return;
    }

    const attributes: DeckdeckgoAttributeDefinition[] | null = await getAttributesDefinition(slide.attributes);

    resolve({
      template: slide.tagName ? slide.tagName.toLowerCase() : undefined,
      content: slide.innerHTML,
      attributes: attributes
    });
  });
}

export function getAttributesDefinition(attributes: NamedNodeMap): Promise<DeckdeckgoAttributeDefinition[] | null> {
  return new Promise<DeckdeckgoAttributeDefinition[] | null>(async (resolve) => {
    if (!attributes || attributes.length <= 0) {
      resolve(null);
      return;
    }

    const results: DeckdeckgoAttributeDefinition[] = [];
    Array.prototype.slice.call(attributes).forEach((attribute: Attr) => {
      if (['id', 'hydrated', 'class', 'contenteditable'].indexOf(attribute.name.toLowerCase()) === -1) {
        let attr: DeckdeckgoAttributeDefinition = {
          name: attribute.name
        };

        if (attribute.value !== undefined) {
          attr.value = `${attribute.value}`;
        }

        results.push(attr);
      }
    });

    resolve(results && results.length > 0 ? results : null);
  });
}

export const isSlide = (slide?: HTMLElement): boolean => {
  return (
    slide?.parentElement?.nodeName.toLowerCase() === 'deckgo-deck' && (!slide.hasAttribute('slot') || slide.getAttribute('slot') === '')
  );
};
