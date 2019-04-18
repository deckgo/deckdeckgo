import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

import {Subscription} from 'rxjs';

import {Utils} from '../../../utils/core/utils';

import {RemoteService} from '../../../services/editor/remote/remote.service';

export class RemoteEventsHandler {

    private el: HTMLElement;

    private remoteService: RemoteService;

    private subscription: Subscription;

    constructor() {
        this.remoteService = RemoteService.getInstance();
    }

    init(el: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.el = el;

            await this.initRemote();

            this.subscription = this.remoteService.watch().subscribe(async (enable: boolean) => {
                if (enable) {
                    await this.connect();
                } else {
                    await this.disconnect();
                }
            });

            resolve();
        });
    }

    async destroy() {
        await this.disconnect();

        if (this.subscription) {
            this.subscription.unsubscribe();
        }

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
            deck.removeEventListener('slidesDidLoad', async ($event: CustomEvent) => {
                await this.initRemoteSlides($event)
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

    private initRemote(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement || !window) {
                resolve();
                return;
            }

            deckgoRemoteElement.server = EnvironmentConfigService.getInstance().get('signalingServerUrl');

            deckgoRemoteElement.addEventListener('event', async ($event) => {
                await this.remoteEvent($event)
            });

            window.addEventListener('resize', Utils.debounce(async () => {
                await this.initRemoteSize();
            }, 300));

            await this.initSlidesDidLoadListener();

            await this.initDeckMove();

            await this.remoteService.init();

            resolve();
        });
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

            const deck = this.el.querySelector('deckgo-deck');

            if (!deckgoRemoteElement || !deck) {
                resolve();
                return;
            }

            const firstSlide = deck.firstElementChild;

            if (!firstSlide) {
                resolve();
                return;
            }

            deckgoRemoteElement.width = firstSlide.clientWidth;
            deckgoRemoteElement.height = firstSlide.clientHeight;

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

    private initSlidesDidLoadListener() {
        return new Promise(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            deck.addEventListener('slidesDidLoad', async ($event: CustomEvent) => {
                await this.initRemoteSize();

                await this.initRemoteSlides($event);
            });

            resolve();
        });
    }

    private initRemoteSlides = ($event: CustomEvent) => {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement || !document || !$event || !$event.detail) {
                resolve();
                return;
            }

            deckgoRemoteElement.slides = $event.detail;

            await this.updateRemoteSlides($event);

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

            const slide: HTMLElement = this.el.querySelector('deckgo-deck > :nth-child(' + (slideIndex + 1) + ')');

            // If slide has no id, it's a new slide, we will slide to the last one in the remote
            if (slide.getAttribute('slide_id')) {
                await deckgoRemoteElement.slideTo(slideIndex, 0);
            }

            resolve();
        });
    }

    private connect(): Promise<void> {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            const room: string = await this.remoteService.getRoom();

            deckgoRemoteElement.room = room;

            await deckgoRemoteElement.connect();

            const deckElement = this.el.querySelector('deckgo-deck');

            if (!deckElement) {
                resolve();
                return;
            }

            await deckElement.slideTo(0, 300, false);

            resolve();
        });
    }

    private disconnect(): Promise<void> {
        return new Promise(async (resolve) => {
            const deckgoRemoteElement: any = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            await deckgoRemoteElement.disconnect();

            resolve();
        });
    }

    private updateRemoteSlides($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck || !$event || !$event.detail || !deck.hasChildNodes()) {
                resolve();
                return;
            }

            const lastSlide: Node = deck.lastChild;

            // If lastSlide has already an id, it means it isn't a new slide, so no need to update the remote
            if (!lastSlide || !(lastSlide instanceof HTMLElement) || lastSlide.getAttribute('slide_id')) {
                resolve();
                return;
            }

            const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            await deckgoRemoteElement.updateSlides();

            resolve();
        });
    }
}
