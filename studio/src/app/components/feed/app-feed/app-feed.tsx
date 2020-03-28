import {Component, h, State, Host} from '@stencil/core';

import {Subscription} from 'rxjs';

import {isMobile} from '@deckdeckgo/utils';

import {Deck} from '../../../models/data/deck';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

import {FeedService} from '../../../services/data/feed/feed.service';
import {OfflineService} from '../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-feed',
  styleUrl: 'app-feed.scss',
  shadow: false
})
export class AppFeed {
  private feedService: FeedService;

  @State()
  private decks: Deck[] = [];

  @State()
  private lastPageReached: boolean = false;

  @State()
  private decksFetched: boolean = false;

  @State()
  private mobile: boolean = false;

  @State()
  private offline: OfflineDeck = undefined;

  private presentationUrl: string = EnvironmentConfigService.getInstance().get('deckdeckgo').presentationUrl;

  private subscription: Subscription;
  private lastPageSubscription: Subscription;

  private offlineSubscription: Subscription;
  private offlineService: OfflineService;

  constructor() {
    this.feedService = FeedService.getInstance();
    this.offlineService = OfflineService.getInstance();
  }

  async componentWillLoad() {
    this.subscription = this.feedService.watchDecks().subscribe((decks: Deck[]) => {
      this.decks = decks;
      this.decksFetched = true;
    });

    this.lastPageSubscription = this.feedService.watchLastPageReached().subscribe((lastPageReached: boolean) => {
      this.lastPageReached = lastPageReached;
      this.decksFetched = lastPageReached;
    });

    this.offlineSubscription = this.offlineService.watchOffline().subscribe((offline: OfflineDeck | undefined) => {
      this.offline = navigator && !navigator.onLine && offline !== undefined ? offline : undefined;
    });

    this.mobile = isMobile();
  }

  async componentDidLoad() {
    await this.feedService.find();
  }

  async componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }

    if (this.lastPageSubscription) {
      this.lastPageSubscription.unsubscribe();
    }

    if (this.offlineSubscription) {
      this.offlineSubscription.unsubscribe();
    }
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

        <ion-infinite-scroll onIonInfinite={($event: CustomEvent) => this.findNextDecks($event)} disabled={this.lastPageReached}>
          <ion-infinite-scroll-content></ion-infinite-scroll-content>
        </ion-infinite-scroll>
      </div>,

      <app-popular help={true}></app-popular>
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
    if (this.decks && this.decks.length > 0) {
      return this.decks.map((deck: Deck, i: number) => {
        return (
          <a href={this.presentationUrl + deck.data.meta.pathname} aria-label={deck.data.meta.title} target="_blank">
            <app-feed-card compact={i > 0} deck={deck}></app-feed-card>
          </a>
        );
      });
    } else {
      return this.renderLoading();
    }
  }

  private renderLoading() {
    if (this.decksFetched) {
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
