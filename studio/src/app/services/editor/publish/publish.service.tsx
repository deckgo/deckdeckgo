import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {Observable, Subject} from 'rxjs';
import {take} from 'rxjs/operators';

import {Deck, DeckMetaAuthor} from '../../../models/data/deck';
import {ApiDeck} from '../../../models/api/api.deck';
import {Slide} from '../../../models/data/slide';
import {User} from '../../../models/data/user';

import {DeckEditorService} from '../deck/deck-editor.service';
import {ApiDeckService} from '../../api/deck/api.deck.service';
import {ApiSlideService} from '../../api/slide/api.slide.service';
import {ApiSlide} from '../../../models/api/api.slide';
import {DeckService} from '../../data/deck/deck.service';
import {SlideService} from '../../data/slide/slide.service';
import {UserService} from '../../data/user/user.service';

export class PublishService {

    private static instance: PublishService;

    private deckEditorService: DeckEditorService;

    private apiDeckService: ApiDeckService;
    private apiSlideService: ApiSlideService;

    private deckService: DeckService;
    private slideService: SlideService;

    private userService: UserService;

    private progressSubject: Subject<number> = new Subject<number>();

    private constructor() {
        // Private constructor, singleton
        this.deckEditorService = DeckEditorService.getInstance();

        this.apiDeckService = ApiDeckService.getInstance();
        this.apiSlideService = ApiSlideService.getInstance();

        this.deckService = DeckService.getInstance();
        this.slideService = SlideService.getInstance();

        this.userService = UserService.getInstance();
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
        return new Promise<string>((resolve, reject) => {
            this.progress(0);

            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                try {
                    if (!deck || !deck.id || !deck.data) {
                        this.progressComplete();
                        reject('No deck found');
                        return;
                    }

                    const apiDeck: ApiDeck = await this.createOrUpdateApiDeck(deck);

                    this.progress(0.15);

                    const newApiId: boolean = deck.data.api_id !== apiDeck.id;
                    if (newApiId) {
                        deck.data.api_id = apiDeck.id;

                        deck = await this.deckService.update(deck);
                    }

                    this.progress(0.3);

                    const apiSlideIds: string[] = await this.createOrUpdateApiSlides(deck);

                    this.progress(0.5);

                    const updatedApiDeck: ApiDeck = await this.putApiDeckSlidesList(apiDeck, apiSlideIds);

                    this.progress(0.65);

                    const publishedUrl: string = await this.apiDeckService.publish(updatedApiDeck);

                    this.progress(0.80);

                    await this.delayUpdateMeta(deck, publishedUrl, description, tags, newApiId);

                    resolve(publishedUrl);
                } catch (err) {
                    this.progressComplete();
                    reject(err);
                }
            });
        });
    }

    // Even if we fixed the delay to publish to Cloudfare CDN (#195), sometimes if too quick, the presentation will not be correctly published
    // Therefore, to avoid such problem, we add a bit of delay in the process but only for the first publish
    private delayUpdateMeta(deck: Deck, publishedUrl: string, description: string, tags: string[], delay: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            setTimeout(() => {
                this.progress(0.9);

                setTimeout(async () => {
                    await this.updateDeckMeta(deck, publishedUrl, description, tags);

                    this.progress(0.95);

                    await this.refreshDeck(deck.id);

                    this.progressComplete();

                    setTimeout(() => {
                        resolve();
                    }, 500);

                }, delay ? 3500 : 0);

            }, delay ? 3500 : 0);
        });
    }

    // Otherwise we gonna kept in memory references like firebase.firestore.FieldValue.delete instead of null values
    private refreshDeck(deckId: string): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                const freshDeck: Deck = await this.deckService.get(deckId);
                this.deckEditorService.next(freshDeck);

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

                this.userService.watch().pipe(take(1)).subscribe(async (user: User) => {
                    const url: URL = new URL(publishedUrl);
                    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

                    if (!deck.data.meta) {
                        deck.data.meta = {
                            title: deck.data.name,
                            pathname: url.pathname,
                            published: true,
                            published_at: now,
                            updated_at: now
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
                                name: user.data.name
                            };
                        } else {
                            (deck.data.meta.author as DeckMetaAuthor).name = user.data.name
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

    private createOrUpdateApiDeck(deck: Deck): Promise<ApiDeck> {
        return new Promise<ApiDeck>(async (resolve, reject) => {
            try {
                let persistedApiDeck: ApiDeck;

                if (deck.data.api_id && deck.data.api_id !== undefined && deck.data.api_id !== '') {
                    const currentDeck: ApiDeck = await this.apiDeckService.get(deck.data.api_id);

                    if (!currentDeck) {
                        persistedApiDeck = await this.postApiDeck(deck);
                    } else {
                        persistedApiDeck = await this.putApiDeck(currentDeck, deck);
                    }
                } else {
                    persistedApiDeck = await this.postApiDeck(deck);
                }

                resolve(persistedApiDeck);
            } catch (err) {
                reject(err);
            }
        })
    }

    private putApiDeck(currentDeck: ApiDeck, deck: Deck): Promise<ApiDeck> {
        return new Promise<ApiDeck>(async (resolve, reject) => {
            try {
                currentDeck.name = deck.data.name;
                currentDeck.attributes = deck.data.attributes;
                currentDeck.background = deck.data.background;

                const persistedDeck: ApiDeck = await this.apiDeckService.put(currentDeck);

                resolve(persistedDeck);
            } catch (err) {
                reject(err);
            }
        });
    }

    private postApiDeck(deck: Deck): Promise<ApiDeck> {
        return new Promise<ApiDeck>(async (resolve, reject) => {
            try {
                const apiDeck: ApiDeck = {
                    slides: [],
                    name: deck.data.name,
                    owner_id: deck.data.owner_id,
                    attributes: deck.data.attributes,
                    background: deck.data.background
                };

                const persistedDeck: ApiDeck = await this.apiDeckService.post(apiDeck);

                resolve(persistedDeck);
            } catch (err) {
                reject(err);
            }
        });
    }

    private putApiDeckSlidesList(apiDeck: ApiDeck, apiSlideIds: string[]): Promise<ApiDeck> {
        return new Promise<ApiDeck>(async (resolve, reject) => {
            try {
                apiDeck.slides = apiSlideIds;

                const persistedDeck: ApiDeck = await this.apiDeckService.put(apiDeck);

                resolve(persistedDeck);
            } catch (err) {
                reject(err);
            }
        });
    }

    private createOrUpdateApiSlides(deck: Deck): Promise<string[]> {
        return new Promise<string[]>(async (resolve, reject) => {
            if (!deck.data.slides || deck.data.slides.length <= 0) {
                resolve([]);
                return;
            }

            try {
                const apiSlideIds: string[] = [];

                for (const slideId in deck.data.slides) {
                    const apiSlideId: string = await this.fetchAndCreateOrUpdateSlide(deck, slideId);
                    apiSlideIds.push(apiSlideId);
                }

                resolve(apiSlideIds);
            } catch (err) {
                reject(err);
            }
        });
    }

    private fetchAndCreateOrUpdateSlide(deck: Deck, slideId: string): Promise<string> {
        return new Promise<string>(async (resolve, reject) => {
            try {
                const slide: Slide = await this.slideService.get(deck.id, slideId);

                if (!slide || !slide.data) {
                    reject('Missing slide for publishing');
                    return;
                }

                const apiSlide: ApiSlide = await this.createOrUpdateApiSlide(deck.data.api_id, slide);

                if (!apiSlide) {
                    reject('Slide could not be created or updated');
                    return;
                }

                if (slide.data.api_id !== apiSlide.id) {
                    slide.data.api_id = apiSlide.id;
                    await this.slideService.update(deck.id, slide);
                }

                resolve(apiSlide.id);
            } catch (err) {
                reject(err);
            }
        });
    }

    private createOrUpdateApiSlide(apiDeckId: string, slide: Slide): Promise<ApiSlide> {
        return new Promise<ApiSlide>(async (resolve, reject) => {
            try {
                let persistedApiSlide: ApiSlide;

                if (slide.data.api_id && slide.data.api_id !== undefined && slide.data.api_id !== '') {
                    const currentSlide: ApiSlide = await this.apiSlideService.get(apiDeckId, slide.data.api_id);

                    if (!currentSlide) {
                        persistedApiSlide = await this.postApiSlide(apiDeckId, slide);
                    } else {
                        persistedApiSlide = await this.putApiSlide(apiDeckId, currentSlide, slide);
                    }
                } else {
                    persistedApiSlide = await this.postApiSlide(apiDeckId, slide);
                }

                resolve(persistedApiSlide);
            } catch (err) {
                reject(err);
            }
        })
    }

    private putApiSlide(apiDeckId: string, currentSlide: ApiSlide, slide: Slide): Promise<ApiSlide> {
        return new Promise<ApiSlide>(async (resolve, reject) => {
            try {
                currentSlide.content = slide.data.content;
                currentSlide.attributes = slide.data.attributes;

                const persistedSlide: ApiSlide = await this.apiSlideService.put(apiDeckId, currentSlide);

                resolve(persistedSlide);
            } catch (err) {
                reject(err);
            }
        });
    }

    private postApiSlide(apiDeckId: string, slide: Slide): Promise<ApiSlide> {
        return new Promise<ApiSlide>(async (resolve, reject) => {
            try {
                const apiSlide: ApiSlide = {
                    template: slide.data.template,
                    content: slide.data.content,
                    attributes: slide.data.attributes
                };

                const persistedSlide: ApiSlide = await this.apiSlideService.post(apiDeckId, apiSlide);

                resolve(persistedSlide);
            } catch (err) {
                reject(err);
            }
        });
    }
}
