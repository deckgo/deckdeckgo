import {ItemReorderEventDetail} from '@ionic/core';

import {debounce} from '@deckdeckgo/utils';

import deckStore from '../../../../stores/deck.store';
import errorStore from '../../../../stores/error.store';
import busyStore from '../../../../stores/busy.store';
import authStore from '../../../../stores/auth.store';

import {cleanContent} from '@deckdeckgo/deck-utils';

import * as firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckAttributes, DeckData} from '../../../../models/data/deck';
import {Slide, SlideAttributes, SlideAttributesYAxisDomain, SlideChartType, SlideData, SlideSplitType, SlideTemplate} from '../../../../models/data/slide';

import {Utils} from '../../../../utils/core/utils';
import {Resources} from '../../../../utils/core/resources';

import {SlotUtils} from '../../../../utils/editor/slot.utils';
import {SlotType} from '../../../../utils/editor/slot-type';
import {ParseElementsUtils} from '../../../../utils/editor/parse-elements.utils';

import {DeckService} from '../../../../services/data/deck/deck.service';
import {SlideService} from '../../../../services/data/slide/slide.service';
import {DeckAction} from '../../../../utils/editor/deck-action';

export class DeckEventsHandler {
  private el: HTMLElement;

  private deckService: DeckService;
  private slideService: SlideService;

  private readonly debounceUpdateSlide: (slide: HTMLElement) => void;
  private readonly debounceUpdateDeckTitle: (title: string) => void;

  constructor() {
    this.deckService = DeckService.getInstance();
    this.slideService = SlideService.getInstance();

    this.debounceUpdateSlide = debounce(async (element: HTMLElement) => {
      await this.updateSlide(element);

      await this.emitSlideDidUpdate(element);
    }, 500);

    this.debounceUpdateDeckTitle = debounce(async (title: string) => {
      await this.updateDeckTitle(title);
    }, 500);
  }

  init(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.el = el;

      this.el.addEventListener('input', this.onInputChange, false);
      this.el.addEventListener('slideDidChange', this.onSlideChange, false);
      this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
      this.el.addEventListener('slidesDidLoad', this.onSlidesDidLoad, false);
      this.el.addEventListener('slideDelete', this.onSlideDelete, false);
      this.el.addEventListener('codeDidChange', this.onCustomEventChange, false);
      this.el.addEventListener('mathDidChange', this.onCustomEventChange, false);
      this.el.addEventListener('imgDidChange', this.onCustomEventChange, false);
      this.el.addEventListener('linkCreated', this.onCustomEventChange, false);
      this.el.addEventListener('styleDidChange', this.onCustomEventChange, false);
      this.el.addEventListener('drrDidChange', this.onCustomEventChange, false);
      this.el.addEventListener('notesDidChange', this.onSlideChange, false);

      if (document) {
        document.addEventListener('deckDidChange', this.onDeckChange, false);
        document.addEventListener('deckNeedChange', this.onDeckNeedChange, false);
      }

      resolve();
    });
  }

  destroy() {
    this.el.removeEventListener('input', this.onInputChange, true);
    this.el.removeEventListener('slideDidChange', this.onSlideChange, true);
    this.el.removeEventListener('slideDidLoad', this.onSlideDidLoad, true);
    this.el.removeEventListener('slidesDidLoad', this.onSlidesDidLoad, true);
    this.el.removeEventListener('slideDelete', this.onSlideDelete, true);
    this.el.removeEventListener('codeDidChange', this.onCustomEventChange, true);
    this.el.removeEventListener('mathDidChange', this.onCustomEventChange, true);
    this.el.removeEventListener('imgDidChange', this.onCustomEventChange, true);
    this.el.removeEventListener('linkCreated', this.onCustomEventChange, true);
    this.el.removeEventListener('styleDidChange', this.onCustomEventChange, true);
    this.el.removeEventListener('drrDidChange', this.onCustomEventChange, true);
    this.el.removeEventListener('notesDidChange', this.onSlideChange, true);

    if (document) {
      document.removeEventListener('deckDidChange', this.onDeckChange, true);
      document.removeEventListener('deckNeedChange', this.onDeckNeedChange, true);
    }
  }

  private onSlideDidLoad = async ($event: CustomEvent) => {
    if ($event && $event.target && $event.target instanceof HTMLElement) {
      await this.createSlide($event.target);
    }
  };

  private onSlidesDidLoad = async ($event: CustomEvent) => {
    if ($event) {
      await this.initSlideSize();
      await this.slideToLastSlide();
    }
  };

  private onDeckChange = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.updateDeck($event.detail);
  };

  private onDeckNeedChange = async ($event: CustomEvent<DeckAction>) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.updateDeckAction($event.detail);
  };

  private onSlideChange = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    this.debounceUpdateSlide($event.detail);
  };

  private onCustomEventChange = async ($event: CustomEvent) => {
    if (!$event || !$event.detail || !($event.detail instanceof HTMLElement)) {
      return;
    }

    const element: HTMLElement = $event.detail as HTMLElement;

    let parent: HTMLElement = element.parentElement;

    if (SlotUtils.isNodeReveal(parent) || SlotUtils.isNodeDragDropResize(parent)) {
      parent = parent.parentElement;
    }

    if (!parent || !parent.nodeName || parent.nodeName.toLowerCase().indexOf('deckgo-slide') <= -1) {
      return;
    }

    this.debounceUpdateSlide(parent);
  };

  private onInputChange = async ($event: Event) => {
    if (!$event || !$event.target || !($event.target instanceof HTMLElement)) {
      return;
    }

    const element: HTMLElement = $event.target as HTMLElement;

    let parent: HTMLElement = element.parentElement;

    if (SlotUtils.isNodeReveal(parent) || SlotUtils.isNodeDragDropResize(parent)) {
      parent = parent.parentElement;
    }

    if (!parent || !parent.nodeName || parent.nodeName.toLowerCase().indexOf('deckgo-slide') <= -1) {
      return;
    }

    this.debounceUpdateSlide(parent);

    // The first content editable element on the first slide is the title of the presentation
    if (parent && !parent.previousElementSibling && !element.previousElementSibling) {
      this.debounceUpdateDeckTitle(element.textContent);
    }
  };

  private onSlideDelete = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.deleteSlide($event.detail);
  };

  private createSlide(slide: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        if (!slide || !slide.nodeName) {
          resolve();
          return;
        }

        if (slide.getAttribute('slide_id')) {
          // !isNew
          await this.contentEditable(slide);

          resolve();
          return;
        }

        busyStore.state.deckBusy = true;

        if (!deckStore.state.deck) {
          const persistedDeck: Deck = await this.createDeck();
          deckStore.state.deck = {...persistedDeck};
        }

        const persistedSlide: Slide = await this.postSlide(deckStore.state.deck, slide);

        // Because of the offline mode, is kind of handy to handle the list on the client side too.
        // But maybe in the future it is something which could be moved to the cloud.
        await this.updateDeckSlideList(deckStore.state.deck, persistedSlide);

        busyStore.state.deckBusy = false;

        resolve();
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve();
      }
    });
  }

  private postSlide(deck: Deck, slide: HTMLElement): Promise<Slide> {
    return new Promise<Slide>(async (resolve) => {
      const slideData: SlideData = {
        template: this.getSlideTemplate(slide),
      };

      const content: string = await this.cleanSlideContent(slide);
      if (content && content.length > 0) {
        slideData.content = content;
      }

      const attributes: SlideAttributes = await this.getSlideAttributes(slide, false);

      if (attributes && Object.keys(attributes).length > 0) {
        slideData.attributes = attributes;
      }

      const persistedSlide: Slide = await this.slideService.create(deck.id, slideData);

      if (persistedSlide && persistedSlide.id) {
        slide.setAttribute('slide_id', persistedSlide.id);

        await this.contentEditable(slide);
      }

      resolve(persistedSlide);
    });
  }

  private createDeck(): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        if (!authStore.state.authUser) {
          reject('User not authenticated');
          return;
        }

        let deck: DeckData = {
          name: `Presentation ${await Utils.getNow()}`,
          owner_id: authStore.state.authUser.uid,
        };

        // Retrieve text and background color style randomly generated in the editor
        const deckElement: HTMLElement = this.el.querySelector('deckgo-deck');
        if (deckElement) {
          const attributes: DeckAttributes = await this.getDeckAttributes(deckElement, false);
          deck.attributes = attributes;
        }

        const persistedDeck: Deck = await this.deckService.create(deck);
        deckStore.state.deck = {...persistedDeck};

        await this.updateNavigation(persistedDeck);

        resolve(persistedDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  private updateDeckSlideList(deck: Deck, slide: Slide): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!deck && !deck.data) {
          reject('Missing deck to add the slide to the list');
          return;
        }

        if (!slide || !slide.id || !slide.data) {
          reject('Missing slide to create or update the deck');
          return;
        }

        if (!deck.data.slides || deck.data.slides.length <= 0) {
          deck.data.slides = [];
        }

        deck.data.slides.push(slide.id);

        const updatedDeck: Deck = await this.deckService.update(deck);
        deckStore.state.deck = {...updatedDeck};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private updateNavigation(deck: Deck): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!deck || !deck.id) {
        resolve();
        return;
      }

      history.replaceState({}, `Deck edited ${deck.id}`, `/editor/${deck.id}`);

      resolve();
    });
  }

  private updateDeck(deck: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        if (!deck) {
          resolve();
          return;
        }

        busyStore.state.deckBusy = true;

        const currentDeck: Deck | null = deckStore.state.deck;

        if (!currentDeck || !currentDeck.data) {
          resolve();
          return;
        }

        const attributes: DeckAttributes = await this.getDeckAttributes(deck, true, currentDeck);

        // @ts-ignore
        currentDeck.data.attributes = attributes && Object.keys(attributes).length > 0 ? attributes : firebase.firestore.FieldValue.delete();

        const slotsPromises: Promise<string>[] = ['background', 'header', 'footer'].map((slotName: 'background' | 'header' | 'footer') => {
          return this.getDeckSlot(deck, slotName);
        });
        const [background, header, footer] = await Promise.all(slotsPromises);

        // @ts-ignore
        currentDeck.data.background = background && background !== undefined && background !== '' ? background : firebase.firestore.FieldValue.delete();

        // @ts-ignore
        currentDeck.data.header = header && header !== undefined && header !== '' ? header : firebase.firestore.FieldValue.delete();

        // @ts-ignore
        currentDeck.data.footer = footer && footer !== undefined && footer !== '' ? footer : firebase.firestore.FieldValue.delete();

        const updatedDeck: Deck = await this.deckService.update(currentDeck);

        deckStore.state.deck = {...updatedDeck};

        busyStore.state.deckBusy = false;

        resolve();
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve();
      }
    });
  }

  private updateDeckTitle(title: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        if (!title || title === undefined || title === '') {
          resolve();
          return;
        }

        busyStore.state.deckBusy = true;

        const currentDeck: Deck | null = deckStore.state.deck;

        if (!currentDeck || !currentDeck.data) {
          resolve();
          return;
        }

        // TODO: Add a check, we should not update the title from the slide in case it would have been set in the publication

        if (title.length >= Resources.Constants.DECK.TITLE_MAX_LENGTH) {
          title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
        }

        currentDeck.data.name = title;

        const updatedDeck: Deck = await this.deckService.update(currentDeck);
        deckStore.state.deck = {...updatedDeck};

        busyStore.state.deckBusy = false;

        resolve();
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve();
      }
    });
  }

  private async updateDeckAction(action: DeckAction): Promise<void> {
    try {
      if (!action || action === undefined) {
        return;
      }

      if (action.autoSlide === undefined) {
        return;
      }

      busyStore.state.deckBusy = true;

      const currentDeck: Deck | null = deckStore.state.deck;

      if (!currentDeck || !currentDeck.data) {
        return;
      }

      if (currentDeck.data.attributes === undefined) {
        currentDeck.data.attributes = {};
      }

      currentDeck.data.attributes.autoSlide = action.autoSlide;

      const updatedDeck: Deck = await this.deckService.update(currentDeck);
      deckStore.state.deck = {...updatedDeck};

      busyStore.state.deckBusy = false;
    } catch (err) {
      errorStore.state.error = err;
      busyStore.state.deckBusy = false;
    }
  }

  private updateSlide(slide: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        if (!slide || !slide.nodeName) {
          resolve();
          return;
        }

        if (!slide.getAttribute('slide_id')) {
          errorStore.state.error = 'Slide is not defined';
          resolve();
          return;
        }

        const slideUpdate: Slide = {
          id: slide.getAttribute('slide_id'),
          data: {
            template: this.getSlideTemplate(slide),
          },
        };

        const content: string = await this.cleanSlideContent(slide);
        if (content && content.length > 0) {
          slideUpdate.data.content = content;
        } else {
          // @ts-ignore
          slideUpdate.data.content = firebase.firestore.FieldValue.delete();
        }

        const attributes: SlideAttributes = await this.getSlideAttributes(slide, true);

        if (attributes && Object.keys(attributes).length > 0) {
          slideUpdate.data.attributes = attributes;
        }

        if (deckStore.state.deck) {
          await this.slideService.update(deckStore.state.deck.id, slideUpdate);
        }

        busyStore.state.deckBusy = false;

        resolve();
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve();
      }
    });
  }

  private deleteSlide(slide: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        if (!slide) {
          resolve();
          return;
        }

        if (!slide.getAttribute('slide_id')) {
          errorStore.state.error = 'Slide is not defined';
          resolve();
          return;
        }

        const slideId: string = slide.getAttribute('slide_id');

        const currentDeck: Deck | null = deckStore.state.deck;

        if (currentDeck && currentDeck.data) {
          const slide: Slide = await this.slideService.get(currentDeck.id, slideId);

          if (slide && slide.data) {
            // Because there is an offline mode, it is handy currently to proceed the following on the client side.
            // But at some point, it might be interesting to move the logic to a cloud function.

            // 1. Delete the slide in Firestore or locally
            await this.slideService.delete(currentDeck.id, slideId);

            // 2. Update list of slide in the deck (in Firestore)
            if (currentDeck.data.slides && currentDeck.data.slides.indexOf(slideId) > -1) {
              currentDeck.data.slides.splice(currentDeck.data.slides.indexOf(slideId), 1);

              const updatedDeck: Deck = await this.deckService.update(currentDeck);
              deckStore.state.deck = {...updatedDeck};
            }
          }
        }

        await this.deleteSlideElement();

        busyStore.state.deckBusy = false;

        resolve();
      } catch (err) {
        errorStore.state.error = err;
        busyStore.state.deckBusy = false;
        resolve();
      }
    });
  }

  private deleteSlideElement(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        resolve();
        return;
      }

      await (deck as any).deleteActiveSlide();

      resolve();
    });
  }

  private getSlideAttributes(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>(async (resolve) => {
      let attributes: SlideAttributes = {};

      if (slide.getAttribute('style')) {
        attributes.style = slide.getAttribute('style');
      } else if (cleanFields) {
        // @ts-ignore
        attributes.style = firebase.firestore.FieldValue.delete();
      }

      if ((slide as any).src) {
        attributes.src = (slide as any).src;
      }

      if (slide.hasAttribute('custom-background')) {
        attributes.customBackground = '' + true;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.customBackground = firebase.firestore.FieldValue.delete();
      }

      if ((slide as any).imgSrc) {
        attributes.imgSrc = (slide as any).imgSrc;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.imgSrc = firebase.firestore.FieldValue.delete();
      }

      if ((slide as any).imgAlt) {
        attributes.imgAlt = (slide as any).imgAlt;
      }

      const qrCodeAttributes: SlideAttributes = await this.getSlideAttributesQRCode(slide, cleanFields);
      const chartAttributes: SlideAttributes = await this.getSlideAttributesChart(slide, cleanFields);
      const splitAttributes: SlideAttributes = await this.getSlideAttributesSplit(slide, cleanFields);
      const authorAttributes: SlideAttributes = await this.getSlideAttributesAuthor(slide, cleanFields);
      const playgroundAttributes: SlideAttributes = await this.getSlideAttributesPlayground(slide, cleanFields);

      attributes = {...attributes, ...qrCodeAttributes, ...chartAttributes, ...splitAttributes, ...authorAttributes, ...playgroundAttributes};

      resolve(attributes);
    });
  }

  private getSlideAttributesSplit(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>((resolve) => {
      if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-split') {
        resolve({});
        return;
      }

      let attributes: SlideAttributes = {};

      if ((slide as any).vertical) {
        attributes.vertical = true;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.vertical = firebase.firestore.FieldValue.delete();
      }

      // We only want to persist the currently specific 'demo' type, not the default one
      if (slide.hasAttribute('type') && (slide.getAttribute('type') as SlideSplitType) === SlideSplitType.DEMO) {
        attributes.type = SlideSplitType.DEMO;
      }

      resolve(attributes);
    });
  }

  private getSlideAttributesAuthor(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>((resolve) => {
      if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-author') {
        resolve({});
        return;
      }

      let attributes: SlideAttributes = {};

      if (slide.hasAttribute('img-mode') && slide.getAttribute('img-mode') !== 'cover') {
        attributes.imgMode = slide.getAttribute('img-mode');
      } else if (cleanFields) {
        // @ts-ignore
        attributes.imgMode = firebase.firestore.FieldValue.delete();
      }

      resolve(attributes);
    });
  }

  private async getSlideAttributesPlayground(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-playground') {
      return {};
    }

    let attributes: SlideAttributes = {};

    if (slide.hasAttribute('theme')) {
      attributes.theme = slide.getAttribute('theme');
    } else if (cleanFields) {
      // @ts-ignore
      attributes.theme = firebase.firestore.FieldValue.delete();
    }

    return attributes;
  }

  private getSlideAttributesQRCode(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>((resolve) => {
      if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-qrcode') {
        resolve({});
        return;
      }

      let attributes: SlideAttributes = {};

      if (slide.hasAttribute('custom-qrcode')) {
        attributes.customQRCode = true;
        attributes.content = (slide as any).content;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.customQRCode = firebase.firestore.FieldValue.delete();
        // @ts-ignore
        attributes.content = firebase.firestore.FieldValue.delete();
      }

      resolve(attributes);
    });
  }

  private getSlideAttributesChart(slide: HTMLElement, cleanFields: boolean): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>((resolve) => {
      if (!slide || !slide.nodeName || slide.nodeName.toLowerCase() !== 'deckgo-slide-chart') {
        resolve({});
        return;
      }

      let attributes: SlideAttributes = {};

      if (slide.hasAttribute('inner-radius')) {
        attributes.innerRadius = parseInt(slide.getAttribute('inner-radius'));
      } else if (cleanFields) {
        // @ts-ignore
        attributes.innerRadius = firebase.firestore.FieldValue.delete();
      }

      if (slide.hasAttribute('type')) {
        attributes.type = SlideChartType[slide.getAttribute('type').toUpperCase()];
      }

      if (slide.hasAttribute('animation')) {
        attributes.animation = slide.hasAttribute('animation');
      }

      if (slide.hasAttribute('date-pattern')) {
        attributes.datePattern = slide.getAttribute('date-pattern');
      } else if (cleanFields) {
        // @ts-ignore
        attributes.datePattern = firebase.firestore.FieldValue.delete();
      }

      if (slide.hasAttribute('y-axis-domain')) {
        attributes.yAxisDomain = slide.getAttribute('y-axis-domain') as SlideAttributesYAxisDomain;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.yAxisDomain = firebase.firestore.FieldValue.delete();
      }

      if (slide.getAttribute('smooth') === 'false') {
        attributes.smooth = false;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.smooth = firebase.firestore.FieldValue.delete();
      }

      if (slide.getAttribute('area') === 'false') {
        attributes.area = false;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.area = firebase.firestore.FieldValue.delete();
      }

      if (slide.hasAttribute('ticks')) {
        attributes.ticks = parseInt(slide.getAttribute('ticks'));
      } else if (cleanFields) {
        // @ts-ignore
        attributes.ticks = firebase.firestore.FieldValue.delete();
      }

      if (slide.getAttribute('grid') === 'true') {
        attributes.grid = true;
      } else if (cleanFields) {
        // @ts-ignore
        attributes.grid = firebase.firestore.FieldValue.delete();
      }

      if (slide.hasAttribute('separator')) {
        attributes.separator = slide.getAttribute('separator');
      } else if (cleanFields) {
        // @ts-ignore
        attributes.separator = firebase.firestore.FieldValue.delete();
      }

      resolve(attributes);
    });
  }

  private getDeckAttributes(deck: HTMLElement, updateDeck: boolean, currentDeck: Deck | null = null): Promise<DeckAttributes> {
    return new Promise<DeckAttributes>((resolve) => {
      let attributes: DeckAttributes = {};

      if (deck.hasAttribute('style') && deck.getAttribute('style') !== '') {
        attributes.style = deck.getAttribute('style');
      } else if (updateDeck) {
        // @ts-ignore
        attributes.style = firebase.firestore.FieldValue.delete();
      }

      if (deck.hasAttribute('animation') && deck.getAttribute('animation') !== 'slide') {
        attributes.animation = deck.getAttribute('animation') as 'slide' | 'fade' | 'none';
      } else if (updateDeck) {
        // @ts-ignore
        attributes.animation = firebase.firestore.FieldValue.delete();
      }

      if (deck.hasAttribute('direction') && deck.getAttribute('direction') !== 'horizontal') {
        attributes.direction = deck.getAttribute('direction') as 'horizontal' | 'vertical' | 'papyrus';
      } else if (updateDeck) {
        // @ts-ignore
        attributes.direction = firebase.firestore.FieldValue.delete();
      }

      if (deck.hasAttribute('direction-mobile') && deck.getAttribute('direction-mobile') !== 'papyrus') {
        attributes.directionMobile = deck.getAttribute('direction-mobile') as 'horizontal' | 'vertical' | 'papyrus';
      } else if (updateDeck) {
        // @ts-ignore
        attributes.directionMobile = firebase.firestore.FieldValue.delete();
      }

      if (currentDeck && currentDeck.data && currentDeck.data.attributes) {
        if (currentDeck.data.attributes.autoSlide !== undefined) {
          attributes.autoSlide = currentDeck.data.attributes.autoSlide;
        } else {
          // @ts-ignore
          attributes.autoSlide = firebase.firestore.FieldValue.delete();
        }
      }

      resolve(attributes);
    });
  }

  private async getDeckSlot(deck: HTMLElement, slotName: 'background' | 'header' | 'footer'): Promise<string | null> {
    const slotElement: HTMLElement = deck.querySelector(`:scope > [slot='${slotName}']`);

    if (!slotElement) {
      return null;
    }

    return cleanContent(slotElement.innerHTML);
  }

  private cleanSlideContent(slide: HTMLElement): Promise<string> {
    return new Promise<string>(async (resolve) => {
      const content: string = await this.filterSlideContentSlots(slide);

      if (!content || content.length <= 0) {
        resolve(content);
        return;
      }

      let result: string = await cleanContent(content);

      result = await this.cleanSlideCustomSlots(slide, result, 'background');
      result = await this.cleanSlideCustomSlots(slide, result, 'header');
      result = await this.cleanSlideCustomSlots(slide, result, 'footer');

      resolve(result);
    });
  }

  private async cleanSlideCustomSlots(slide: HTMLElement, content: string, customAttribute: 'background' | 'header' | 'footer'): Promise<string> {
    if (!slide.hasAttribute(`custom-${customAttribute}`)) {
      const regex: RegExp = new RegExp(`<div slot="${customAttribute}"(.*?)<\/div>`, 'g');
      content = content.replace(regex, '');
    }

    return content;
  }

  private filterSlideContentSlots(slide: HTMLElement): Promise<string> {
    return new Promise<string>((resolve) => {
      if (!slide || !document) {
        resolve(null);
        return;
      }

      const div = document.createElement('div');

      const elements: HTMLElement[] = Array.prototype.slice.call(slide.childNodes);
      elements.forEach((e: HTMLElement) => {
        if (e.nodeName && e.nodeType === 1 && e.hasAttribute('slot')) {
          div.appendChild(e.cloneNode(true));
        }
      });

      if (!div || div.childElementCount <= 0) {
        resolve(null);
        return;
      }

      const content: string = div.innerHTML;

      resolve(content);
    });
  }

  private getSlideTemplate(slide: HTMLElement): SlideTemplate {
    const templateKey: string = Object.keys(SlideTemplate).find((key: string) => {
      return slide.nodeName.toLowerCase().indexOf(SlideTemplate[key]) > -1;
    });

    return SlideTemplate[templateKey];
  }

  private slideToLastSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck || !deck.children || deck.children.length <= 0) {
        resolve();
        return;
      }

      const slides: Element[] = Array.from(deck.children).filter((slide: Element) => {
        return slide.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1;
      });

      if (!slides || slides.length <= 0) {
        resolve();
        return;
      }

      const lastSlide: Element = slides[slides.length - 1];

      if (!lastSlide || lastSlide.getAttribute('slide_id')) {
        resolve();
        return;
      }

      await (deck as any).slideTo(slides.length - 1);

      resolve();
    });
  }

  initSlideSize(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        return;
      }

      await (deck as any).initSlideSize();

      resolve();
    });
  }

  updateDeckSlidesOrder(detail: ItemReorderEventDetail): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!detail) {
          reject('No new order provided for the slides');
          return;
        }

        if (detail.from < 0 || detail.to < 0 || detail.from === detail.to) {
          reject('The new order provided for the slides is not valid');
          return;
        }

        const currentDeck: Deck | null = deckStore.state.deck;

        if (currentDeck && currentDeck.data && currentDeck.data.slides && detail.to < currentDeck.data.slides.length) {
          currentDeck.data.slides.splice(detail.to, 0, ...currentDeck.data.slides.splice(detail.from, 1));

          const updatedDeck: Deck = await this.deckService.update(currentDeck);
          deckStore.state.deck = {...updatedDeck};
        }

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private emitSlideDidUpdate(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slideDidUpdate: CustomEvent<HTMLElement> = new CustomEvent<HTMLElement>('slideDidUpdate', {
        bubbles: true,
        detail: element,
      });

      this.el.dispatchEvent(slideDidUpdate);

      resolve();
    });
  }

  private async contentEditable(slide: HTMLElement): Promise<void> {
    if (!slide || slide.childElementCount <= 0) {
      busyStore.state.slidesEditable = true;
      return;
    }

    const elements: HTMLElement[] = Array.prototype.slice.call(slide.childNodes);

    elements.forEach((e: HTMLElement) => {
      if (e.nodeName && e.nodeType === 1 && e.hasAttribute('slot')) {
        if (e.nodeName.toLowerCase() === SlotType.CODE || e.nodeName.toLowerCase() === SlotType.MATH) {
          e.setAttribute('editable', '');
        } else if (ParseElementsUtils.isElementContentEditable(e)) {
          e.setAttribute('contentEditable', '');
        } else if (SlotUtils.isNodeReveal(e) && e.firstElementChild) {
          e.firstElementChild.setAttribute('contentEditable', '');
        }
      }
    });

    busyStore.state.slidesEditable = true;
  }
}
