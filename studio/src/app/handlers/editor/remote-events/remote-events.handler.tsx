import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {filter, take} from 'rxjs/operators';

import {Deck} from '../../../models/deck';

import {Utils} from '../../../utils/core/utils';

import {DeckEditorService} from '../../../services/deck/deck-editor.service';

export class RemoteEventsHandler {

    private el: HTMLElement;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    init(el: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.el = el;

            const deckgoRemoteElement: HTMLElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement || !window) {
                resolve();
                return;
            }

            deckgoRemoteElement.addEventListener('event', async ($event) => {
                await this.remoteEvent($event)
            });

            window.addEventListener('resize', Utils.debounce(async () => {
                await this.initRemoteSize();
            }, 300));
            
            await this.initDeckRemote();
            
            await this.initDeckMove();
            
            resolve();
        });
    }

    destroy() {
        const deckgoRemoteElement: HTMLElement = this.el.querySelector('deckgo-remote');

        if (deckgoRemoteElement) {
            deckgoRemoteElement.removeEventListener('event', async ($event) => {
                await this.remoteEvent($event)
            });
        }

        if (window) {
            window.removeEventListener('resize', async () => {
                await this.initRemoteSize();
            });
        }

        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (deck) {
            deck.removeEventListener('slidesDidLoad', async (slides) => {
                await this.initRemoteRoomServer(slides)
            });

            deck.removeEventListener('slideNextDidChange', async () => {
                await this.slidePrevNext(true)
            });

            deck.removeEventListener('slidePrevDidChange', async () => {
                await this.slidePrevNext(false)
            });

            deck.removeEventListener('slideWillChange', async (event) => {
                await this.moveRemote(event)
            });

            deck.removeEventListener('slideDrag', async (event) => {
                await this.scrollRemote(event)
            });

            deck.removeEventListener('slideToChange', async (event) => {
                await this.slideToChange(event)
            });
        }


    }

    remoteEvent = async ($event) => {
        return new Promise(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            const deck = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            const type = $event.detail.type;

            if (type === 'next_slide') {
                const slideAnimation = $event.detail.slideAnimation;
                await deck.slideNext(slideAnimation, slideAnimation);
            } else if (type === 'prev_slide') {
                const slideAnimation = $event.detail.slideAnimation;
                await deck.slidePrev(slideAnimation, slideAnimation);
            } else if (type === 'slide_action') {
                await this.youtubePlayPause($event);
            } else if (type === 'slide_to') {
                const index = $event.detail.index;
                if (index >= 0) {
                    await deck.slideTo(index, 0);
                }
            }

            resolve();
        });
    };

    private initRemoteSize = async () => {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            const deck = this.el.querySelector('deckgo-deck > *:first-child');

            if (!deckgoRemoteElement || !deck) {
                resolve();
                return;
            }

            deckgoRemoteElement.width = deck.clientWidth;
            deckgoRemoteElement.height = deck.clientHeight;

            deckgoRemoteElement.length = deck.childElementCount;

            resolve();
        });
    };

    private youtubePlayPause($event) {
        return new Promise(async (resolve) => {
            const deck = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            const index = await deck.getActiveIndex();

            const youtubeSlideElement: any = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

            if (!youtubeSlideElement || youtubeSlideElement.tagName !== 'deckgo-slide-youtube'.toUpperCase()) {
                resolve();
                return;
            }

            if ($event.detail.action === 'youtube_pause') {
                await youtubeSlideElement.pause();
            } else {
                await youtubeSlideElement.play();
            }

            resolve();
        });
    }

    private initDeckRemote() {
        return new Promise(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            deck.addEventListener('slidesDidLoad', async (slides) => {
                await this.initRemoteSize();

                await this.initRemoteRoomServer(slides)
            });

            resolve();
        });
    }

    private initRemoteRoomServer = (slides) => {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement || !document) {
                resolve();
                return;
            }

            deckgoRemoteElement.slides = slides.detail;

            deckgoRemoteElement.server = EnvironmentConfigService.getInstance().get('signalingServerUrl');

            this.deckEditorService.watch().pipe(filter((deck: Deck) => deck && (deck.name && deck.name !== undefined && deck.name !== '')), take(1)).subscribe(async (deck: Deck) => {
                deckgoRemoteElement.room = deck.name;
            });

            resolve();
        });
    };
    
    private initDeckMove() {
        return new Promise(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            deck.addEventListener('slideNextDidChange', async () => {
                await this.slidePrevNext(true)
            });

            deck.addEventListener('slidePrevDidChange', async () => {
                await this.slidePrevNext(false)
            });

            deck.addEventListener('slideWillChange', async (event) => {
                await this.moveRemote(event)
            });

            deck.addEventListener('slideDrag', async (event) => {
                await this.scrollRemote(event)
            });

            deck.addEventListener('slideToChange', async (event) => {
                await this.slideToChange(event)
            });

            resolve();
        });
    }
    
    private slidePrevNext(next) {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            if (next) {
                await deckgoRemoteElement.nextSlide();
            } else {
                await deckgoRemoteElement.prevSlide();
            }

            resolve();
        });
    }
    
    private moveRemote($event) {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            await deckgoRemoteElement.moveDraw($event.detail, '300ms');

            resolve();
        });
    }

    private scrollRemote($event) {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            await deckgoRemoteElement.moveDraw($event.detail, '0ms');

            resolve();
        });
    }
    
    private slideToChange($event) {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement || !event) {
                resolve();
                return;
            }

            const slideIndex = $event.detail;

            await deckgoRemoteElement.slideTo(slideIndex, 0);

            resolve();
        });
    }

}
