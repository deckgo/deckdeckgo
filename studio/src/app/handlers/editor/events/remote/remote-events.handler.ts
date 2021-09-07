import {Build} from '@stencil/core';

import remoteStore from '../../../../stores/remote.store';

import {componentOnReady} from '../../../../utils/ionic/ionic.overlay';

import {debounce} from '@deckdeckgo/utils';
import {getSlideDefinition} from '@deckdeckgo/deck-utils';
import {ConnectionState, DeckdeckgoDeckDefinition, DeckdeckgoEventDeckRequest, DeckdeckgoSlideDefinition} from '@deckdeckgo/types';

import {EnvironmentDeckDeckGoConfig} from '../../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../../services/environment/environment-config.service';

import {deckSelector, selectSlide} from '../../../../utils/editor/deck.utils';

import {RemoteService} from '../../../../services/editor/remote/remote.service';

export class RemoteEventsHandler {
  private el: HTMLElement;

  private remoteService: RemoteService;

  private destroyConnectListener;
  private destroyAcceptRequestListener;

  constructor() {
    this.remoteService = RemoteService.getInstance();
  }

  async init(el: HTMLElement) {
    this.el = el;

    const remote: HTMLDeckgoRemoteElement | null = this.el.querySelector('deckgo-remote');

    componentOnReady(remote, async () => {
      await this.initRemote();

      this.destroyConnectListener = remoteStore.onChange('remote', async (enable: boolean) => {
        if (enable) {
          await this.connect();
        } else {
          await this.disconnect();
        }
      });

      this.destroyAcceptRequestListener = remoteStore.onChange('acceptedRequest', async (request: DeckdeckgoEventDeckRequest) => {
        await this.startAcceptedRemoteRequest(request);
      });
    });
  }

  async destroy() {
    if (!this.el) {
      return;
    }

    await this.disconnect();

    if (this.destroyConnectListener) {
      this.destroyConnectListener();
    }

    if (this.destroyAcceptRequestListener) {
      this.destroyAcceptRequestListener();
    }

    const deckgoRemoteElement: HTMLElement = this.el.querySelector('deckgo-remote');

    if (deckgoRemoteElement) {
      deckgoRemoteElement.removeEventListener('event', async ($event) => {
        await this.remoteEvent($event);
      });

      deckgoRemoteElement.removeEventListener('state', async ($event) => {
        await this.remoteState($event);
      });
    }

    if (window) {
      window.removeEventListener('resize', async () => {
        await this.initRemoteSize();
      });
    }

    const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

    if (deck) {
      deck.removeEventListener('slidesDidLoad', async ($event: CustomEvent) => {
        await this.initRemoteSlides($event);
      });

      deck.removeEventListener('slideNextDidChange', async () => {
        await this.slidePrevNext(true, false);
      });

      deck.removeEventListener('slidePrevDidChange', async () => {
        await this.slidePrevNext(false, false);
      });

      deck.removeEventListener('slideNextDidAnimate', async () => {
        await this.slidePrevNext(true, true);
      });

      deck.removeEventListener('slidePrevDidAnimate', async () => {
        await this.slidePrevNext(false, true);
      });

      deck.removeEventListener('slideWillChange', async ($event) => {
        await this.moveRemote($event);
      });

      deck.removeEventListener('slideDrag', async ($event) => {
        await this.scrollRemote($event);
      });

      deck.removeEventListener('slideToChange', async ($event) => {
        await this.slideToChange($event);
      });
    }

    this.el.removeEventListener('slideDelete', this.onSlideDelete, true);
    this.el.removeEventListener('slideDidUpdate', this.slideDidUpdate, true);
    this.el.removeEventListener('pollUpdated', this.pollUpdated, true);
    this.el.removeEventListener('deckDidChange', this.deckDidChange, true);
  }

  private initRemote(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement || !window) {
        resolve();
        return;
      }

      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      deckgoRemoteElement.socketUrl = config.socketUrl;

      deckgoRemoteElement.addEventListener('event', async ($event) => {
        await this.remoteEvent($event);
      });

      deckgoRemoteElement.addEventListener('state', async ($event) => {
        await this.remoteState($event);
      });

      window.addEventListener(
        'resize',
        debounce(async () => {
          await this.initRemoteSize();
        }, 300)
      );

      await this.initSlidesDidLoadListener();

      await this.initDeckMove();

      this.el.addEventListener('slideDelete', this.onSlideDelete, false);
      this.el.addEventListener('slideDidUpdate', this.slideDidUpdate, false);
      this.el.addEventListener('pollUpdated', this.pollUpdated, false);
      this.el.addEventListener('deckDidChange', this.deckDidChange, false);

      resolve();
    });
  }

  private remoteState = async ($event) => {
    if (!$event || !$event.detail) {
      return;
    }

    this.remoteService.nextState($event.detail);
  };

  private remoteEvent = async ($event) => {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck) {
        resolve();
        return;
      }

      const type = $event.detail.type;

      if (type === 'next_slide') {
        const slideAnimation = $event.detail.slideAnimation;
        await deck.slideNext(slideAnimation, false);
        await this.emitRemoteSlideDidChange();
      } else if (type === 'prev_slide') {
        const slideAnimation = $event.detail.slideAnimation;
        await deck.slidePrev(slideAnimation, false);
        await this.emitRemoteSlideDidChange();
      } else if (type === 'slide_action') {
        await this.youtubePlayPause($event);
      } else if (type === 'slide_to') {
        const index = $event.detail.index;
        if (index >= 0) {
          await deck.slideTo(index, 0);
          await this.emitRemoteSlideDidChange();
        }
      } else if (type === 'deck_request') {
        await this.remoteService.addPendingRequests($event.detail);
      }

      resolve();
    });
  };

  private initRemoteSize = async () => {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

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
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck) {
        resolve();
        return;
      }

      const index = await deck.getActiveIndex();

      const youtubeSlideElement: HTMLDeckgoSlideYoutubeElement | null = selectSlide({deck, index}) as HTMLDeckgoSlideYoutubeElement | null;

      if (!youtubeSlideElement || youtubeSlideElement.tagName !== 'deckgo-slide-youtube'.toUpperCase()) {
        resolve();
        return;
      }

      if ($event.detail.action === 'pause') {
        await youtubeSlideElement.pause();
      } else {
        await youtubeSlideElement.play();
      }

      resolve();
    });
  }

  private initSlidesDidLoadListener() {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck) {
        resolve();
        return;
      }

      deck.addEventListener('slidesDidLoad', async ($event: CustomEvent) => {
        await this.remoteService.init();

        await this.initRemoteSize();

        await this.initRemoteSlides($event);
      });

      resolve();
    });
  }

  private initRemoteSlides = async ($event: CustomEvent) => {
    const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = this.el.querySelector('deckgo-remote');

    if (!deckgoRemoteElement || !document || !$event || !$event.detail) {
      return;
    }

    deckgoRemoteElement.deck = $event.detail;

    await this.updateRemoteSlidesOnSlidesDidLoad($event);
  };

  private initDeckMove() {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck) {
        resolve();
        return;
      }

      deck.addEventListener('slideNextDidChange', async () => {
        await this.slidePrevNext(true, false);
      });

      deck.addEventListener('slidePrevDidChange', async () => {
        await this.slidePrevNext(false, false);
      });

      deck.addEventListener('slideNextDidAnimate', async () => {
        await this.slidePrevNext(true, true);
      });

      deck.addEventListener('slidePrevDidAnimate', async () => {
        await this.slidePrevNext(false, true);
      });

      deck.addEventListener('slideWillChange', async ($event) => {
        await this.moveRemote($event);
      });

      deck.addEventListener('slideDrag', async ($event) => {
        await this.scrollRemote($event);
      });

      deck.addEventListener('slideToChange', async ($event) => {
        await this.slideToChange($event);
      });

      resolve();
    });
  }

  private slidePrevNext(next: boolean, animation: boolean) {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      if (next) {
        await deckgoRemoteElement.nextSlide(animation);
      } else {
        await deckgoRemoteElement.prevSlide(animation);
      }

      resolve();
    });
  }

  private moveRemote($event) {
    return new Promise<void>(async (resolve) => {
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
    return new Promise<void>(async (resolve) => {
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
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement || !event) {
        resolve();
        return;
      }

      const slideIndex = $event.detail;

      const slide: HTMLElement = this.el.querySelector('deckgo-deck > :nth-child(' + (slideIndex + 1) + ')');

      // If slide has no id, it's a new slide, we will slide to the last one in the remote
      if (slide && slide.getAttribute('slide_id')) {
        await deckgoRemoteElement.slideTo(slideIndex, 0);
      }

      resolve();
    });
  }

  private async connect() {
    const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = this.el.querySelector('deckgo-remote');

    if (!deckgoRemoteElement) {
      return;
    }

    const room: string = await this.remoteService.getRoom();

    if (!room) {
      return;
    }

    deckgoRemoteElement.room = room;

    await deckgoRemoteElement.connect();

    const deckElement: HTMLDeckgoDeckElement | null = document.querySelector(deckSelector);

    if (!deckElement) {
      return;
    }

    await deckElement.slideTo(0, 300, false);
  }

  private disconnect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement: HTMLDeckgoRemoteElement = this.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      if (Build.isServer) {
        resolve();
        return;
      }

      await deckgoRemoteElement.disconnect();

      resolve();
    });
  }

  private updateRemoteSlidesOnSlidesDidLoad($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

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

      await this.updateRemoteSlides(this);

      resolve();
    });
  }

  updateRemoteSlidesOnMutation(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck || !deck.hasChildNodes()) {
        resolve();
        return;
      }

      const observer: MutationObserver = new MutationObserver(async (_mutations: MutationRecord[], _observer: MutationObserver) => {
        await this.executeIfConnected(this.updateRemoteDeckWithDefinition);

        observer.disconnect();
      });

      observer.observe(deck, {childList: true, subtree: true});

      resolve();
    });
  }

  private updateRemoteDeckWithDefinition(self): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck || !deck.hasChildNodes()) {
        resolve();
        return;
      }

      const deckDefinition: DeckdeckgoDeckDefinition | null = await (deck as HTMLDeckgoDeckElement).getDeckDefinition();

      if (deckDefinition) {
        const deckgoRemoteElement = self.el.querySelector('deckgo-remote');

        if (deckgoRemoteElement) {
          deckgoRemoteElement.deck = deckDefinition;

          await self.updateRemoteSlides(self);
        }
      }

      resolve();
    });
  }

  private updateCurrentSlideWithDefinition(self): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck || !deck.hasChildNodes()) {
        resolve();
        return;
      }

      const index = await (deck as any).getActiveIndex();

      const slideDefinition: DeckdeckgoSlideDefinition | null = await (deck as any).getSlideDefinition(index);

      await self.updateSlideDefinition(self, slideDefinition, index);

      resolve();
    });
  }

  private updatePollSlideWithDefinition(self, slide: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!slide) {
        resolve();
        return;
      }

      const deck: HTMLDeckgoDeckElement = document.querySelector(deckSelector);

      if (!deck || !deck.hasChildNodes()) {
        resolve();
        return;
      }

      const index = Array.prototype.indexOf.call(deck.children, slide);

      const slideDefinition: DeckdeckgoSlideDefinition | null = await getSlideDefinition(slide);

      await self.updateSlideDefinition(self, slideDefinition, index);

      resolve();
    });
  }

  async updateSlideDefinition(self, slideDefinition: DeckdeckgoSlideDefinition | null, index: number) {
    if (slideDefinition) {
      const deckgoRemoteElement = self.el.querySelector('deckgo-remote');

      if (deckgoRemoteElement) {
        await deckgoRemoteElement.updateSlide(index, slideDefinition);
      }
    }
  }

  private updateRemoteSlides(self): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = self.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      await deckgoRemoteElement.updateSlides();

      resolve();
    });
  }

  private onSlideDelete = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.executeIfConnected(this.deleteRemoteSlide);
  };

  private slideDidUpdate = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.executeIfConnected(this.updateCurrentSlideWithDefinition);
  };

  private pollUpdated = async ($event: CustomEvent) => {
    if (!$event || !$event.target) {
      return;
    }

    await this.executeIfConnected(this.updatePollSlideWithDefinition, $event.target);
  };

  private deckDidChange = async ($event: CustomEvent) => {
    if (!$event || !$event.detail) {
      return;
    }

    await this.executeIfConnected(this.updateRemoteDeckWithDefinition);
  };

  private async executeIfConnected(func: (self, options?) => Promise<void>, options?) {
    if (remoteStore.state.remote) {
      await func(this, options);
    }
  }

  private deleteRemoteSlide(self): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckgoRemoteElement = self.el.querySelector('deckgo-remote');

      if (!deckgoRemoteElement) {
        resolve();
        return;
      }

      await deckgoRemoteElement.deleteSlide();

      resolve();
    });
  }

  async updateRemoteReveal(reveal: boolean) {
    const deckgoRemoteElement: HTMLDeckgoRemoteElement | null = this.el.querySelector('deckgo-remote');

    if (!deckgoRemoteElement) {
      return;
    }

    if (remoteStore.state.connectionState === ConnectionState.CONNECTED) {
      await deckgoRemoteElement.updateReveal(reveal);
      return;
    }

    deckgoRemoteElement.deck = {
      ...deckgoRemoteElement.deck,
      reveal: reveal
    };
  }

  private async startAcceptedRemoteRequest(request: DeckdeckgoEventDeckRequest) {
    if (!request || !request.fromSocketId) {
      return;
    }

    const deckgoRemoteElement = this.el.querySelector('deckgo-remote');

    if (!deckgoRemoteElement) {
      return;
    }

    await deckgoRemoteElement.start(request.fromSocketId);
  }

  private async emitRemoteSlideDidChange() {
    const slideDidChange: CustomEvent<void> = new CustomEvent<void>('remoteSlideDidChange', {
      bubbles: true
    });

    this.el.dispatchEvent(slideDidChange);
  }
}
