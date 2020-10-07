import {Component, h, State, Host} from '@stencil/core';

import {isMobile} from '@deckdeckgo/utils';

import feedStore from '../../../stores/feed.store';
import offlineStore from '../../../stores/offline.store';

import {Deck} from '../../../models/data/deck';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

import {FeedService} from '../../../services/data/feed/feed.service';

@Component({
  tag: 'app-feed',
  styleUrl: 'app-feed.scss',
  shadow: false,
})
export class AppFeed {
  private feedService: FeedService;

  @State()
  private mobile: boolean = false;

  @State()
  private offline: OfflineDeck = undefined;

  private presentationUrl: string = EnvironmentConfigService.getInstance().get('deckdeckgo').presentationUrl;

  private destroyListener;

  constructor() {
    this.feedService = FeedService.getInstance();
  }

  async componentWillLoad() {
    this.initOffline(offlineStore.state.offline);

    this.destroyListener = offlineStore.onChange('offline', (offline: OfflineDeck | undefined) => {
      this.initOffline(offline);

      this.destroyListener();
    });

    this.mobile = isMobile();
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private initOffline(offline: OfflineDeck | undefined) {
    this.offline = navigator && !navigator.onLine && offline !== undefined ? offline : undefined;
  }

  async componentDidLoad() {
    await this.feedService.find();
  }

  private async findNextDecks($event) {
    setTimeout(async () => {
      await this.feedService.find();
      $event.target.complete();
    }, 500);
  }

  private async refreshFeed($event) {
    if (!$event || !$event.target) {
      return;
    }

    await this.feedService.refresh();

    $event.target.complete();
  }

  render() {
    return (
      <Host>
        {this.renderRefresher()}

        <main class={this.offline ? 'offline' : undefined}>
          {this.renderFeed()}

          {this.renderOffline()}
        </main>
      </Host>
    );
  }

  private renderFeed() {
    if (this.offline !== undefined) {
      return undefined;
    }

    return [
      <app-popular description={true}></app-popular>,

      <div class="feed">
        {this.renderDecks()}

        <ion-infinite-scroll onIonInfinite={($event: CustomEvent) => this.findNextDecks($event)} disabled={feedStore.state.lastPageReached}>
          <ion-infinite-scroll-content></ion-infinite-scroll-content>
        </ion-infinite-scroll>
      </div>,

      <app-popular help={true}></app-popular>,
    ];
  }

  private renderRefresher() {
    if (this.mobile) {
      return (
        <ion-refresher slot="fixed" onIonRefresh={($event: CustomEvent) => this.refreshFeed($event)}>
          <ion-refresher-content></ion-refresher-content>
        </ion-refresher>
      );
    } else {
      // Only on mobile devices otherwise conflict with click action to open presentations from cards
      return undefined;
    }
  }

  private renderDecks() {
    if (feedStore.state.decks && feedStore.state.decks.length > 0) {
      return feedStore.state.decks.map((deck: Deck, i: number) => {
        return (
          <a href={this.presentationUrl + deck.data.meta.pathname} aria-label={deck.data.meta.title} target="_blank" rel="noopener noreferrer">
            <app-feed-card compact={i > 0} deck={deck}></app-feed-card>
          </a>
        );
      });
    } else {
      return this.renderLoading();
    }
  }

  private renderLoading() {
    if (feedStore.state.decks !== undefined) {
      return undefined;
    } else {
      return (
        <div class="spinner">
          <ion-spinner color="primary"></ion-spinner>
        </div>
      );
    }
  }

  private renderOffline() {
    if (this.offline === undefined) {
      return undefined;
    }

    return (
      <section>
        <ion-label class="ion-text-center">
          You are editing <strong>{this.offline.name}</strong> offline.
        </ion-label>

        <ion-button shape="round" href={`/editor/${this.offline.id}`} routerDirection="root" color="tertiary" class="ion-margin-top">
          <ion-label>Continue your presentation</ion-label>
        </ion-button>
      </section>
    );
  }
}
