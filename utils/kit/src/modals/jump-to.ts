class SlidesList extends HTMLElement {
  constructor() {
    super();
  }

  async connectedCallback() {
    const slidesListActions = await buildSlidesListActions();

    this.innerHTML = '<ion-list><ion-list-header>Jump to slide</ion-list-header>' + slidesListActions + '</ion-list>';
  }
}

async function buildSlidesListActions(): Promise<string> {
  let result = '';

  const slides: NodeListOf<HTMLElement> = document.querySelectorAll('#slider > *');

  if (slides) {
    let i = 0;

    for (const slide of Array.from(slides)) {
      if (slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
        const text = getSlideTitle(slide, i);

        result += '<ion-item ion-item button color="primary"><ion-label>' + text + '</ion-label></ion-item>';

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
        return 'Slide ' + (index + 1);
      }
    }
  }
}

async function jumpToSlide(index: number) {
  const deck = document.getElementById('slider') as HTMLDeckgoDeckElement | null;
  await deck?.slideTo(index, 0);

  const popover = document.querySelector('ion-popover.access') as HTMLIonPopoverElement;
  await popover?.dismiss();
}

customElements.define('slides-list', SlidesList);

export async function presentSlidePicker() {
  const popover = document.createElement('ion-popover');
  popover.component = 'slides-list';
  popover.translucent = true;
  popover.cssClass = 'menu';

  document.body.appendChild(popover);

  await popover.present();

  await bindSlidesListActions();
}

async function bindSlidesListActions() {
  if (!document) {
    return;
  }

  const items = document.querySelectorAll('slides-list ion-item.ion-activatable');

  if (items) {
    items.forEach((item, index) => {
      item.addEventListener('click', async () => await jumpToSlide(index), true);
    });
  }
}
