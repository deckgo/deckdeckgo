import {JSX} from '@stencil/core';

import store from '../../stores/deck.store';
import errorStore from '../../stores/error.store';
import busyStore from '../../stores/busy.store';

import {Slide} from '../../models/data/slide';
import {Deck} from '../../models/data/deck';

import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';

import {DeckService} from '../../services/data/deck/deck.service';
import {SlideService} from '../../services/data/slide/slide.service';

export class EditorHelper {
  private slideService: SlideService;
  private deckService: DeckService;

  constructor() {
    this.slideService = SlideService.getInstance();

    this.deckService = DeckService.getInstance();
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
        const deck: Deck = await this.deckService.get(deckId);

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
        const slide: Slide = await this.slideService.get(deck.id, slideId);
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
          const slide: Slide = await this.slideService.get(store.state.deck.id, slideId);
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
