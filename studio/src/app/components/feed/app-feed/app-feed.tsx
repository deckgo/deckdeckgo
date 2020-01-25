import {Component, h, State, Host} from '@stencil/core';

import {Subscription} from 'rxjs';

import {Deck} from '../../../models/data/deck';

import {FeedService} from '../../../services/data/feed/feed.service';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {isMobile} from '@deckdeckgo/utils';

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

  private presentationUrl: string = EnvironmentConfigService.getInstance().get('deckdeckgo').presentationUrl;

  private subscription: Subscription;
  private lastPageSubscription: Subscription;

  constructor() {
    this.feedService = FeedService.getInstance();
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

        <main>
          <app-popular description={true}></app-popular>

          <div class="feed">
            {this.renderDecks()}

            <ion-infinite-scroll onIonInfinite={($event: CustomEvent) => this.findNextDecks($event)} disabled={this.lastPageReached}>
              <ion-infinite-scroll-content></ion-infinite-scroll-content>
            </ion-infinite-scroll>
          </div>

          <app-popular help={true}></app-popular>
        </main>
      </Host>
    );
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
}
