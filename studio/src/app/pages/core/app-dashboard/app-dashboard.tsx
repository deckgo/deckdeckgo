import {Component, h, JSX, State} from '@stencil/core';

import {convertStyle} from '@deckdeckgo/deck-utils';

import authStore from '../../../stores/auth.store';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';
import {AuthUser} from '../../../models/auth/auth.user';

import {ParseDeckSlotsUtils} from '../../../utils/editor/parse-deck-slots.utils';

import {ParseSlidesUtils} from '../../../utils/editor/parse-slides.utils';
import {DeckService} from '../../../services/data/deck/deck.service';
import {SlideService} from '../../../services/data/slide/slide.service';

import {DeckDashboardCloneResult, DeckDashboardService} from '../../../services/dashboard/deck/deck-dashboard.service';
import {ImageEventsHandler} from '../../../handlers/core/events/image/image-events.handler';
import {ChartEventsHandler} from '../../../handlers/core/events/chart/chart-events.handler';
import navStore, {NavDirection} from '../../../stores/nav.store';

interface DeckAndFirstSlide {
  deck: Deck;
  slide: JSX.IntrinsicElements | undefined;
  style: any;
  background: JSX.IntrinsicElements | undefined;
  header: JSX.IntrinsicElements | undefined;
  footer: JSX.IntrinsicElements | undefined;
}

@Component({
  tag: 'app-dashboard',
  styleUrl: 'app-dashboard.scss',
})
export class AppDashboard {
  @State()
  private filteredDecks: DeckAndFirstSlide[] = null;

  private decks: DeckAndFirstSlide[] = null;

  private deckService: DeckService;
  private slideService: SlideService;

  private deckDashboardService: DeckDashboardService;

  private imageEventsHandler: ImageEventsHandler = new ImageEventsHandler();
  private chartEventsHandler: ChartEventsHandler = new ChartEventsHandler();

  private destroyListener;

  constructor() {
    this.deckService = DeckService.getInstance();
    this.slideService = SlideService.getInstance();
    this.deckDashboardService = DeckDashboardService.getInstance();
  }

  async componentWillLoad() {
    await this.imageEventsHandler.init();
    await this.chartEventsHandler.init();
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initDashboard();
    });

    await this.initDashboard();
  }

  private async initDashboard() {
    if (!authStore.state.authUser) {
      return;
    }

    this.destroyListener();

    const userDecks: Deck[] = await this.deckService.getUserDecks(authStore.state.authUser.uid);
    this.decks = await this.fetchFirstSlides(userDecks);
    await this.filterDecks(null);

    // If some decks are currently cloned, we watch them to update GUI when clone has finished processing
    await this.initWatchForClonedDecks();
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }

    this.imageEventsHandler.destroy();
    this.chartEventsHandler.destroy();
  }

  private fetchFirstSlides(decks: Deck[]): Promise<DeckAndFirstSlide[]> {
    return new Promise<DeckAndFirstSlide[]>(async (resolve) => {
      if (!decks || decks.length <= 0) {
        resolve([]);
        return;
      }

      const promises = [];
      decks.forEach((deck: Deck) => {
        if (deck && deck.data) {
          if (deck.data.clone?.deck_id_from) {
            promises.push(this.initDeckCloneInProgress(deck));
          } else if (deck.data.slides && deck.data.slides.length >= 1) {
            promises.push(this.initDeckAndFirstSlide(deck, deck.data.slides[0]));
          }
        }
      });

      const results: DeckAndFirstSlide[] = await Promise.all(promises);

      resolve(results);
    });
  }

  private initWatchForClonedDecks(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.decks || this.decks.length <= 0) {
        resolve();
        return;
      }

      const promises: Promise<void>[] = [];

      this.decks.forEach((deck: DeckAndFirstSlide) => {
        if (deck.deck?.data?.clone !== undefined) {
          promises.push(this.deckDashboardService.snapshot(deck.deck, this.watchClonedDeck));
        }
      });

      if (promises.length <= 0) {
        resolve();
        return;
      }

      await Promise.all(promises);

      resolve();
    });
  }

  private initDeckAndFirstSlide(deck: Deck, slideId: string): Promise<DeckAndFirstSlide> {
    return new Promise<DeckAndFirstSlide>(async (resolve) => {
      try {
        const slide: Slide = await this.slideService.get(deck.id, slideId);
        const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(deck, slide, false);

        const style: any = await this.convertStyle(deck);

        const background: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.background, 'background');
        const header: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.header, 'header');
        const footer: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.footer, 'footer');

        resolve({
          deck,
          slide: element,
          style,
          background,
          header,
          footer,
        });
      } catch (err) {
        resolve(undefined);
      }
    });
  }

  private initDeckCloneInProgress(deck: Deck): Promise<DeckAndFirstSlide> {
    return new Promise<DeckAndFirstSlide>(async (resolve) => {
      try {
        const element: JSX.IntrinsicElements = (
          <div class="spinner">
            <ion-spinner color="primary"></ion-spinner>
            <ion-label>Copy in progress...</ion-label>
          </div>
        );

        const style: any = await this.convertStyle(deck);

        const background: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.background, 'background');
        const header: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.header, 'header');
        const footer: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.footer, 'footer');

        resolve({
          deck,
          slide: element,
          style,
          background,
          header,
          footer,
        });
      } catch (err) {
        resolve(undefined);
      }
    });
  }

  private async convertStyle(deck: Deck): Promise<any> {
    let style: any;
    if (deck.data?.attributes?.style) {
      style = await convertStyle(deck.data.attributes.style);
    } else {
      style = undefined;
    }

    return style;
  }

  private filterDecks(value: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!value || value === undefined || value === '') {
        this.filteredDecks = this.decks ? [...this.decks] : [];

        resolve();
        return;
      }

      if (!this.decks || this.decks.length <= 0) {
        this.filteredDecks = this.decks ? [...this.decks] : [];

        resolve();
        return;
      }

      const matchingDecks: DeckAndFirstSlide[] = this.decks.filter((matchDeck: DeckAndFirstSlide) => {
        return matchDeck.deck?.data?.name?.toLowerCase().indexOf(value.toLowerCase()) > -1;
      });

      this.filteredDecks = [...matchingDecks];

      resolve();
    });
  }

  private async signIn() {
    navStore.state.nav = {
      url: '/signin' + (window.location?.pathname ?? ''),
      direction: NavDirection.FORWARD,
    };
  }

  private async filterDecksOnChange(e: CustomEvent) {
    if (e && e.detail) {
      await this.filterDecks(e.detail.value);
    } else {
      await this.filterDecks(null);
    }
  }

  private blockSlide($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if ($event && $event.target) {
        await ($event.target as any).blockSlide(true);
      }

      resolve();
    });
  }

  private navigateDeck(deck: DeckAndFirstSlide) {
    if (!deck || !deck.deck || !deck.deck.id || deck.deck.id === undefined || deck.deck.id === '') {
      return;
    }

    if (deck?.deck?.data?.clone) {
      return;
    }

    const url: string = `/editor/${deck.deck.id}`;

    navStore.state.nav = {
      url: url,
      direction: NavDirection.RELOAD,
    };
  }

  private async navigateEditor() {
    navStore.state.nav = {
      url: '/editor',
      direction: NavDirection.RELOAD,
    };
  }

  private removeDeletedDeck($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail === undefined || $event.detail === '') {
        resolve();
        return;
      }

      const deckId: string = $event.detail;

      const index: number = await this.findDeckIndex(deckId);

      if (index < 0) {
        resolve();
        return;
      }

      this.decks.splice(index, 1);

      await this.filterDecks(null);

      resolve();
    });
  }

  private onClonedDeck($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail === undefined) {
        resolve();
        return;
      }

      const clone: DeckDashboardCloneResult = $event.detail;

      const index: number = await this.findDeckIndex(clone?.from?.id ?? undefined);

      if (index < 0) {
        resolve();
        return;
      }

      await this.updateClonedDeck(clone.from, index);

      await this.addClonedDeck(clone.to, index);

      await this.filterDecks(null);

      await this.deckDashboardService.snapshot(clone.from, this.watchClonedDeck);
      await this.deckDashboardService.snapshot(clone.to, this.watchClonedDeck);

      resolve();
    });
  }

  private findDeckIndex(id: string): Promise<number> {
    return new Promise<number>(async (resolve) => {
      if (!this.decks || this.decks.length < 0) {
        resolve(-1);
        return;
      }

      if (!id || id === undefined || id === '') {
        resolve(-1);
        return;
      }

      const index: number = this.decks.findIndex((matchDeck: DeckAndFirstSlide) => {
        return matchDeck.deck?.id === id;
      });

      resolve(index);
    });
  }

  private updateClonedDeck(deck: Deck, index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!deck.data || !deck.data.slides || deck.data.slides.length <= 0) {
        resolve();
        return;
      }

      this.decks[index] = await this.initDeckAndFirstSlide(deck, deck.data.slides[0]);

      resolve();
    });
  }

  private addClonedDeck(deck: Deck, index: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deckAndFirstSlide: DeckAndFirstSlide = await this.initDeckCloneInProgress(deck);

      if (!this.decks || this.decks.length <= 0) {
        // Well, should not happens
        this.decks = [deckAndFirstSlide];

        resolve();
        return;
      }

      this.decks = [...this.decks.slice(0, index), deckAndFirstSlide, ...this.decks.slice(index)];

      resolve();
    });
  }

  private watchClonedDeck = async (deck: Deck, unsubscribe) => {
    if (!deck || !deck.data) {
      return;
    }

    if (!deck.data.slides || deck.data.slides.length <= 0) {
      return;
    }

    // if element still contains cloned data we don't update it
    if (deck.data.clone !== undefined) {
      return;
    }

    const index: number = await this.findDeckIndex(deck.id);

    if (index < 0) {
      return;
    }

    this.decks[index] = await this.initDeckAndFirstSlide(deck, deck.data.slides[0]);

    await this.filterDecks(null);

    unsubscribe();
  };

  render() {
    return [<app-navigation presentation={true}></app-navigation>, <ion-content class="ion-padding">{this.renderGuardedContent()}</ion-content>];
  }

  private renderGuardedContent() {
    if (!authStore.state.authUser) {
      return this.renderNotLoggedInContent();
    } else {
      return this.renderContent();
    }
  }

  private renderNotLoggedInContent() {
    return (
      <main class="ion-padding">
        <h1>Oh, hi! Good to have you.</h1>
        <p class="ion-padding-top">
          <button type="button" class="app-button" onClick={() => this.signIn()}>
            Sign in
          </button>
          to access to your presentations.
        </p>
      </main>
    );
  }

  private renderContent() {
    if (!this.filteredDecks) {
      return undefined;
    }

    return (
      <main class="ion-padding">
        {this.renderTitle()}
        {this.renderDecksFilter()}
        {this.renderCreateButton()}
        {this.renderDecks()}
      </main>
    );
  }

  private renderTitle() {
    if (this.filteredDecks.length > 0) {
      return <h1>Your presentations</h1>;
    } else {
      return <h1>You don't have any presentation yet</h1>;
    }
  }

  private renderDecksFilter() {
    if (this.filteredDecks.length > 0) {
      return (
        <ion-searchbar
          debounce={500}
          animated={false}
          placeholder="Filter your presentations"
          onClick={($event) => $event.stopImmediatePropagation()}
          onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
          class="ion-no-padding ion-margin-top ion-margin-bottom"
        />
      );
    } else {
      return undefined;
    }
  }

  private renderCreateButton() {
    if (this.filteredDecks.length === 0) {
      return (
        <ion-grid>
          <ion-row class="ion-justify-content-center">
            <ion-column>
              <ion-button slot="end" shape="round" fill="outline" onClick={() => this.navigateEditor()} class="ion-margin-top">
                <ion-label>Start one now 🚀</ion-label>
              </ion-button>
            </ion-column>
          </ion-row>
        </ion-grid>
      );
    } else {
      return undefined;
    }
  }

  private renderDecks() {
    if (this.filteredDecks && this.filteredDecks.length > 0) {
      return <div class="container">{this.renderDecksCards()}</div>;
    } else {
      return undefined;
    }
  }

  private renderDecksCards() {
    return this.filteredDecks.map((deck: DeckAndFirstSlide) => {
      if (deck === undefined) {
        return undefined;
      }

      return (
        <ion-card class="item ion-no-margin" onClick={() => this.navigateDeck(deck)} key={deck.deck.id}>
          {this.renderDeck(deck)}

          <app-dashboard-deck-actions
            deck={deck.deck}
            onDeckDeleted={($event: CustomEvent) => this.removeDeletedDeck($event)}
            onDeckCloned={($event: CustomEvent) => this.onClonedDeck($event)}></app-dashboard-deck-actions>
        </ion-card>
      );
    });
  }

  private renderDeck(deck: DeckAndFirstSlide) {
    if (!deck) {
      return undefined;
    } else {
      return (
        <deckgo-deck embedded={true} keyboard={false} style={deck.style} onSlidesDidLoad={($event: CustomEvent) => this.blockSlide($event)}>
          {deck.slide}
          {deck.background}
          {deck.header}
          {deck.footer}
        </deckgo-deck>
      );
    }
  }
}
