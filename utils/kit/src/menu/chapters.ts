class Chapters extends HTMLElement {
  constructor() {
    super();
  }

  async connectedCallback() {
    const slidesListActions = await buildSlidesListActions();

    this.innerHTML = '<ion-list><ion-list-header><h2>Slides</h2></ion-list-header>' + slidesListActions + '</ion-list>';

    await bindChaptersActions();
  }
}

async function buildSlidesListActions(): Promise<string> {
  let result = '';

  const slides: NodeListOf<HTMLElement> = document.querySelectorAll('#slider > *');

  if (slides) {
    let i = 0;

    for (const slide of Array.from(slides)) {
      if (slide.tagName && (!slide.hasAttribute('slot') || slide.getAttribute('slot') === '')) {
        const text = getSlideTitle(slide, i);

        result += '<ion-item ion-item button><ion-label>' + text + '</ion-label></ion-item>';

        i++;
      }
    }
  }

  return result;
}

function getSlideTitle(slide: HTMLElement, index: number) {
  if (!slide) {
    return 'Slide ' + (index + 1);
  }

  const title = slide.querySelector('[slot="title"],[slot="question"]');

  if (title && title.textContent !== '') {
    return title.textContent;
  } else {
    const start = slide.querySelector('[slot="start"],[slot="header"]');

    if (start && start.textContent !== '') {
      return start.textContent;
    } else {
      const end = slide.querySelector('[slot="end"],[slot="footer"]');

      if (end && end.textContent !== '') {
        return end.textContent;
      } else {
        return 'Slide #' + (index + 1);
      }
    }
  }
}

customElements.get('deckgo-chapters') || customElements.define('deckgo-chapters', Chapters);

export async function presentSlidePicker() {
  const menuButton = document.querySelector('ion-menu-button');
  menuButton?.click();
}

async function bindChaptersActions() {
  const items = document.querySelectorAll('deckgo-chapters ion-item');

  if (items) {
    items.forEach((item, index) => {
      item.addEventListener('click', async () => await jumpToSlide(index), true);
    });
  }
}

async function jumpToSlide(index: number) {
  const deck = document.getElementById('slider') as HTMLDeckgoDeckElement | null;
  await deck?.slideTo(index, 0);

  const menu = document.querySelector('ion-menu') as HTMLIonMenuElement;
  await menu?.close();
}
