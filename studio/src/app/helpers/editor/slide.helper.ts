import {JSX} from '@stencil/core';

import store from '../../stores/deck.store';
import errorStore from '../../stores/error.store';
import busyStore from '../../stores/busy.store';

import {Slide} from '../../models/data/slide';
import {Deck} from '../../models/data/deck';

import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';
import {TemplateUtils} from '../../utils/editor/template.utils';

import {TemplateService} from '../../providers/data/template/template.service';
import {DeckOfflineProvider} from '../../providers/data/deck/deck.offline.provider';
import {SlideOfflineProvider} from '../../providers/data/slide/slide.offline.provider';

export class SlideHelper {
  private deckOfflineProvider: DeckOfflineProvider;
  private slideOfflineProvider: SlideOfflineProvider;
  private templateService: TemplateService;

  constructor() {
    this.slideOfflineProvider = SlideOfflineProvider.getInstance();
    this.deckOfflineProvider = DeckOfflineProvider.getInstance();
    this.templateService = TemplateService.getInstance();
  }

  loadDeckAndRetrieveSlides(deckId: string): Promise<any[]> {
    return new Promise<any[]>(async (resolve) => {
      if (!deckId) {
        errorStore.state.error = 'Deck is not defined';
        resolve(null);
        return;
      }

      busyStore.state.deckBusy = true;

      try {
        const deck: Deck = await this.deckOfflineProvider.get(deckId);

        if (!deck || !deck.data) {
          errorStore.state.error = 'No deck could be fetched';
          resolve(null);
          return;
        }

        store.state.deck = {...deck};

        if (!deck.data.slides || deck.data.slides.length <= 0) {
          resolve([]);
          return;
        }

        await this.templateService.init();

        const promises: Promise<any>[] = [];
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

        busyStore.state.deckBusy = false;

        resolve(parsedSlides);
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve(null);
      }
    });
  }

  private fetchSlide(deck: Deck, slideId: string): Promise<JSX.IntrinsicElements> {
    return new Promise<any>(async (resolve) => {
      try {
        const slide: Slide = await this.slideOfflineProvider.get(deck.id, slideId);

        await TemplateUtils.loadSlideTemplate(slide);

        const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(deck, slide, true);

        resolve(element);
      } catch (err) {
        errorStore.state.error = 'Something went wrong while loading and parsing a slide';
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
          errorStore.state.error = 'Slide is not defined';
          resolve(null);
          return;
        }

        const slideId: string = slide.getAttribute('slide_id');

        let element: JSX.IntrinsicElements = null;

        if (store.state.deck?.data) {
          const slide: Slide = await this.slideOfflineProvider.get(store.state.deck.id, slideId);
          element = await ParseSlidesUtils.parseSlide(store.state.deck, slide, true, true);
        }

        busyStore.state.deckBusy = false;

        resolve(element);
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve(null);
      }
    });
  }
}
