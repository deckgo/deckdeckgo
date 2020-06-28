import * as firebase from 'firebase/app';
import 'firebase/firestore';

import {Observable, Subject} from 'rxjs';
import {take} from 'rxjs/operators';

import store from '../../../stores/deck.store';

import {Deck, DeckMetaAuthor} from '../../../models/data/deck';
import {ApiDeck} from '../../../models/api/api.deck';
import {Slide, SlideAttributes, SlideTemplate} from '../../../models/data/slide';
import {User} from '../../../models/data/user';

import {ApiPresentation} from '../../../models/api/api.presentation';
import {ApiSlide} from '../../../models/api/api.slide';

import {DeckService} from '../../data/deck/deck.service';
import {SlideService} from '../../data/slide/slide.service';
import {UserService} from '../../data/user/user.service';

import {ApiPresentationService} from '../../api/presentation/api.presentation.service';
import {ApiPresentationFactoryService} from '../../api/presentation/api.presentation.factory.service';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';
import {EnvironmentGoogleConfig} from '../../core/environment/environment-config';
import {FontsService} from '../fonts/fonts.service';

export class PublishService {
  private static instance: PublishService;

  private apiPresentationService: ApiPresentationService;

  private deckService: DeckService;
  private slideService: SlideService;

  private userService: UserService;

  private fontsService: FontsService;

  private progressSubject: Subject<number> = new Subject<number>();

  private constructor() {
    this.apiPresentationService = ApiPresentationFactoryService.getInstance();

    this.deckService = DeckService.getInstance();
    this.slideService = SlideService.getInstance();

    this.userService = UserService.getInstance();

    this.fontsService = FontsService.getInstance();
  }

  static getInstance() {
    if (!PublishService.instance) {
      PublishService.instance = new PublishService();
    }
    return PublishService.instance;
  }

  watchProgress(): Observable<number> {
    return this.progressSubject.asObservable();
  }

  private progress(progress: number) {
    this.progressSubject.next(progress);
  }

  private progressComplete() {
    this.progressSubject.next(1);
  }

  // TODO: Move in a cloud functions?
  publish(description: string, tags: string[]): Promise<string> {
    return new Promise<string>(async (resolve, reject) => {
      this.progress(0);

      try {
        if (!store.state.deck || !store.state.deck.id || !store.state.deck.data) {
          this.progressComplete();
          reject('No deck found');
          return;
        }

        const apiDeck: ApiDeck = await this.convertDeck(store.state.deck, description);

        this.progress(0.25);

        const apiDeckPublish: ApiPresentation = await this.publishDeck(store.state.deck, apiDeck);

        this.progress(0.5);

        if (!apiDeckPublish || !apiDeckPublish.id || !apiDeckPublish.url) {
          this.progressComplete();
          reject('Publish failed');
          return;
        }

        this.progress(0.75);

        const newApiId: boolean = store.state.deck.data.api_id !== apiDeckPublish.id;
        if (newApiId) {
          store.state.deck.data.api_id = apiDeckPublish.id;

          const updatedDeck: Deck = await this.deckService.update(store.state.deck);
          store.state.deck = {...updatedDeck};
        }

        this.progress(0.8);

        const publishedUrl: string = apiDeckPublish.url;

        await this.delayUpdateMeta(store.state.deck, publishedUrl, description, tags, newApiId);

        resolve(publishedUrl);
      } catch (err) {
        this.progressComplete();
        reject(err);
      }
    });
  }

  private publishDeck(deck: Deck, apiDeck: ApiDeck): Promise<ApiPresentation> {
    return new Promise<ApiPresentation>(async (resolve, reject) => {
      try {
        const apiDeckPublish: ApiPresentation = await this.createOrUpdatePublish(deck, apiDeck);

        resolve(apiDeckPublish);
      } catch (err) {
        reject(err);
      }
    });
  }

  private createOrUpdatePublish(deck: Deck, apiDeck: ApiDeck): Promise<ApiPresentation> {
    if (deck.data.api_id) {
      return this.apiPresentationService.put(deck.data.api_id, apiDeck);
    } else {
      return this.apiPresentationService.post(apiDeck);
    }
  }

  private convertDeck(deck: Deck, description: string): Promise<ApiDeck> {
    return new Promise<ApiDeck>(async (resolve, reject) => {
      try {
        const apiSlides: ApiSlide[] = await this.convertSlides(deck);

        const apiDeck: ApiDeck = {
          name: deck.data.name ? deck.data.name.trim() : deck.data.name,
          description: description !== undefined && description !== '' ? description : deck.data.name,
          owner_id: deck.data.owner_id,
          attributes: deck.data.attributes,
          background: deck.data.background,
          slides: apiSlides,
        };

        const googleFontScript: string | undefined = await this.getGoogleFontScript(deck);
        if (googleFontScript !== undefined) {
          apiDeck.head_extra = googleFontScript;
        }

        resolve(apiDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  private getGoogleFontScript(deck: Deck): Promise<string | undefined> {
    return new Promise<string | undefined>(async (resolve) => {
      if (!document) {
        resolve(undefined);
        return;
      }
      if (!deck || !deck.data || !deck.data.attributes) {
        resolve(undefined);
        return;
      }

      if (!deck.data.attributes.style || deck.data.attributes.style === undefined || deck.data.attributes.style === '') {
        resolve(undefined);
        return;
      }

      const div: HTMLDivElement = document.createElement('div');
      div.setAttribute('style', deck.data.attributes.style);

      const fontFamily: string | undefined = div.style.getPropertyValue('font-family');

      if (!fontFamily || fontFamily === undefined || fontFamily === '') {
        resolve(undefined);
        return;
      }

      const font: GoogleFont | undefined = await this.fontsService.extractGoogleFont(fontFamily);

      if (!font || font === undefined) {
        resolve(undefined);
        return;
      }

      const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');
      const url: string = this.fontsService.getGoogleFontUrl(google.fontsUrl, font);

      const link = document.createElement('link');
      link.setAttribute('rel', 'stylesheet');
      link.setAttribute('href', url);

      resolve(link.outerHTML);
    });
  }

  private convertSlides(deck: Deck): Promise<ApiSlide[]> {
    return new Promise<ApiSlide[]>(async (resolve, reject) => {
      if (!deck.data.slides || deck.data.slides.length <= 0) {
        resolve([]);
        return;
      }

      try {
        const promises: Promise<ApiSlide>[] = [];

        for (let i: number = 0; i < deck.data.slides.length; i++) {
          const slideId: string = deck.data.slides[i];

          promises.push(this.convertSlide(deck, slideId));
        }

        if (!promises || promises.length <= 0) {
          resolve([]);
          return;
        }

        const slides: ApiSlide[] = await Promise.all(promises);

        resolve(slides);
      } catch (err) {
        reject(err);
      }
    });
  }

  private convertSlide(deck: Deck, slideId: string): Promise<ApiSlide> {
    return new Promise<ApiSlide>(async (resolve, reject) => {
      const slide: Slide = await this.slideService.get(deck.id, slideId);

      if (!slide || !slide.data) {
        reject('Missing slide for publishing');
        return;
      }

      const attributes: SlideAttributes = await this.convertAttributesToString(slide.data.attributes);

      const apiSlide: ApiSlide = {
        template: slide.data.template,
        content: slide.data.content,
        attributes: attributes,
      };

      const cleanApiSlide: ApiSlide = await this.convertSlideQRCode(apiSlide);
      cleanApiSlide.content = await this.cleanNotes(apiSlide.content);

      resolve(cleanApiSlide);
    });
  }

  private convertAttributesToString(attributes: SlideAttributes): Promise<SlideAttributes> {
    return new Promise<SlideAttributes>((resolve) => {
      if (!attributes) {
        resolve(undefined);
        return;
      }

      // We loose the type but doing so we ensure that all attributes are converted to string in order to parse them to HTML in the API
      const result: SlideAttributes = {};
      Object.keys(attributes).forEach((key: string) => {
        result[key] = `${attributes[key]}`;
      });

      if (!result) {
        resolve(undefined);
        return;
      }

      resolve(result);
    });
  }

  private cleanNotes(content: string): Promise<string> {
    return new Promise<string>((resolve) => {
      if (!content || content === undefined || content === '') {
        resolve(content);
        return;
      }

      const result: string = content.replace(/<div slot="notes".*?>(.|[\s\S])*?<\/div>/gi, '');

      resolve(result);
    });
  }

  private convertSlideQRCode(apiSlide: ApiSlide): Promise<ApiSlide> {
    return new Promise<ApiSlide>(async (resolve) => {
      if (!apiSlide) {
        resolve(apiSlide);
        return;
      }

      if (apiSlide.template !== SlideTemplate.QRCODE) {
        resolve(apiSlide);
        return;
      }

      const presentationUrl: string = EnvironmentConfigService.getInstance().get('deckdeckgo').presentationUrl;

      // If no attributes at all, we create an attribute "content" of the QR code with it's upcoming published url
      if (!apiSlide.attributes) {
        apiSlide.attributes = {
          content: `${presentationUrl}{{DECKDECKGO_BASE_HREF}}`,
        };
      }

      // If not custom content, we replace the attribute "content" of the QR code with it's upcoming published url
      if (!apiSlide.attributes.hasOwnProperty('customQRCode') || !apiSlide.attributes.customQRCode) {
        apiSlide.attributes.content = `${presentationUrl}{{DECKDECKGO_BASE_HREF}}`;
      }

      // In any case, we don't need customQRCode attribute in our presentations, this is an attribute used by the editor
      if (apiSlide.attributes.hasOwnProperty('customQRCode')) {
        delete apiSlide.attributes['customQRCode'];
      }

      resolve(apiSlide);
    });
  }

  // Even if we fixed the delay to publish to Cloudfare CDN (#195), sometimes if too quick, the presentation will not be correctly published
  // Therefore, to avoid such problem, we add a bit of delay in the process but only for the first publish
  private delayUpdateMeta(deck: Deck, publishedUrl: string, description: string, tags: string[], delay: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      setTimeout(
        () => {
          this.progress(0.9);

          setTimeout(
            async () => {
              await this.updateDeckMeta(deck, publishedUrl, description, tags);

              this.progress(0.95);

              await this.refreshDeck(deck.id);

              this.progressComplete();

              setTimeout(() => {
                resolve();
              }, 500);
            },
            delay ? 3500 : 0
          );
        },
        delay ? 3500 : 0
      );
    });
  }

  // Otherwise we gonna kept in memory references like firebase.firestore.FieldValue.delete instead of null values
  private refreshDeck(deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const freshDeck: Deck = await this.deckService.get(deckId);
        store.state.deck = {...freshDeck};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private updateDeckMeta(deck: Deck, publishedUrl: string, description: string, tags: string[]): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!publishedUrl || publishedUrl === undefined || publishedUrl === '') {
          resolve();
          return;
        }

        this.userService
          .watch()
          .pipe(take(1))
          .subscribe(async (user: User) => {
            const url: URL = new URL(publishedUrl);
            const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

            if (!deck.data.meta) {
              deck.data.meta = {
                title: deck.data.name,
                pathname: url.pathname,
                published: true,
                published_at: now,
                feed: true,
                updated_at: now,
              };
            } else {
              deck.data.meta.title = deck.data.name;
              deck.data.meta.pathname = url.pathname;
              deck.data.meta.updated_at = now;
            }

            if (description && description !== undefined && description !== '') {
              deck.data.meta.description = description;
            } else {
              deck.data.meta.description = firebase.firestore.FieldValue.delete();
            }

            if (!tags || tags.length <= 0) {
              deck.data.meta.tags = firebase.firestore.FieldValue.delete();
            } else {
              deck.data.meta.tags = tags;
            }

            if (user && user.data && user.data.name) {
              if (!deck.data.meta.author) {
                deck.data.meta.author = {
                  name: user.data.name,
                };
              } else {
                (deck.data.meta.author as DeckMetaAuthor).name = user.data.name;
              }

              if (user.data.photo_url) {
                (deck.data.meta.author as DeckMetaAuthor).photo_url = user.data.photo_url;
              }
            } else {
              if (deck.data.meta.author) {
                deck.data.meta.author = firebase.firestore.FieldValue.delete();
              }
            }

            await this.deckService.update(deck);

            resolve();
          });
      } catch (err) {
        reject(err);
      }
    });
  }
}
