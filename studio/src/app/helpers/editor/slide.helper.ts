import {Deck, Slide} from '@deckdeckgo/editor';
import {errorStore, editorStore, busyStore} from '@deckdeckgo/studio';
import {JSX} from '@stencil/core';
import {DeckOfflineProvider} from '../../providers/data/deck/deck.offline.provider';
import {SlideOfflineProvider} from '../../providers/data/slide/slide.offline.provider';
import {initTemplates} from '../../providers/data/template/template.provider';
import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';
import {TemplateUtils} from '../../utils/editor/template.utils';

export class SlideHelper {
  private deckOfflineProvider: DeckOfflineProvider;
  private slideOfflineProvider: SlideOfflineProvider;

  constructor() {
    this.slideOfflineProvider = SlideOfflineProvider.getInstance();
    this.deckOfflineProvider = DeckOfflineProvider.getInstance();
  }

  loadDeckAndRetrieveSlides(deckId: string): Promise<JSX.IntrinsicElements[]> {
    return new Promise<JSX.IntrinsicElements[]>(async (resolve) => {
      if (!deckId) {
        errorStore.default.state.error = 'Deck is not defined';
        resolve(null);
        return;
      }

      busyStore.default.state.busy = true;

      try {
        const deck: Deck = await this.deckOfflineProvider.get(deckId);

        if (!deck || !deck.data) {
          errorStore.default.state.error = 'No deck could be fetched';
          resolve(null);
          return;
        }

        editorStore.default.state.deck = {...deck};

        if (!deck.data.slides || deck.data.slides.length <= 0) {
          resolve([]);
          return;
        }

        await initTemplates();

        const promises: Promise<JSX.IntrinsicElements>[] = [];
        deck.data.slides.forEach((slideId: string) => {
          promises.push(this.fetchSlide(deck, slideId));
        });

        let parsedSlides: any[] = [];
        if (promises.length > 0) {
          parsedSlides = await Promise.all(promises);
        }

        if (!parsedSlides || parsedSlides.length <= 0) {
          resolve([]);
          return;
        }

        busyStore.default.state.busy = false;

        resolve(parsedSlides);
      } catch (err) {
        errorStore.default.state.error = err;
        busyStore.default.state.busy = false;
        resolve(null);
      }
    });
  }

  private fetchSlide(deck: Deck, slideId: string): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      try {
        const slide: Slide = await this.slideOfflineProvider.get(deck.id, slideId);

        await TemplateUtils.loadSlideTemplate(slide);

        const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(slide, true);

        resolve(element);
      } catch (err) {
        errorStore.default.state.error = 'Something went wrong while loading and parsing a slide';
        resolve(null);
      }
    });
  }

  copySlide(slide: HTMLElement): Promise<JSX.IntrinsicElements> {
    return new Promise<JSX.IntrinsicElements>(async (resolve) => {
      try {
        if (!slide) {
          resolve(null);
          return;
        }

        if (!slide.getAttribute('slide_id')) {
          errorStore.default.state.error = 'Slide is not defined';
          resolve(null);
          return;
        }

        const slideId: string = slide.getAttribute('slide_id');

        let element: JSX.IntrinsicElements = null;

        if (editorStore.default.state.deck?.data) {
          const slide: Slide = await this.slideOfflineProvider.get(editorStore.default.state.deck.id, slideId);
          element = await ParseSlidesUtils.parseSlide(slide, true, true);
        }

        busyStore.default.state.busy = false;

        resolve(element);
      } catch (err) {
        errorStore.default.state.error = err;
        busyStore.default.state.busy = false;
        resolve(null);
      }
    });
  }
}
