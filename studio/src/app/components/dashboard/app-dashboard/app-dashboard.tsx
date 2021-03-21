import {Component, Fragment, h, JSX, State} from '@stencil/core';

import {convertStyle} from '@deckdeckgo/deck-utils';

import authStore from '../../../stores/auth.store';
import navStore, {NavDirection} from '../../../stores/nav.store';
import errorStore from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';
import {AuthUser} from '../../../models/auth/auth.user';

import {signIn} from '../../../utils/core/signin.utils';
import {renderI18n} from '../../../utils/core/i18n.utils';
import {ParseDeckSlotsUtils} from '../../../utils/editor/parse-deck-slots.utils';
import {ParseSlidesUtils} from '../../../utils/editor/parse-slides.utils';
import {TemplateUtils} from '../../../utils/editor/template.utils';

import {DeckService} from '../../../services/data/deck/deck.service';
import {SlideService} from '../../../services/data/slide/slide.service';
import {DeckDashboardCloneResult, DeckDashboardService} from '../../../services/dashboard/deck/deck-dashboard.service';
import {TemplateService} from '../../../services/data/template/template.service';

import {ImageEventsHandler} from '../../../handlers/core/events/image/image-events.handler';
import {ChartEventsHandler} from '../../../handlers/core/events/chart/chart-events.handler';

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

  @State()
  private loading: boolean;

  private decks: DeckAndFirstSlide[] = null;

  private readonly deckService: DeckService;
  private readonly slideService: SlideService;
  private readonly deckDashboardService: DeckDashboardService;
  private readonly templateService: TemplateService;

  private imageEventsHandler: ImageEventsHandler = new ImageEventsHandler();
  private chartEventsHandler: ChartEventsHandler = new ChartEventsHandler();

  private destroyListener;

  constructor() {
    this.deckService = DeckService.getInstance();
    this.slideService = SlideService.getInstance();
    this.deckDashboardService = DeckDashboardService.getInstance();
    this.templateService = TemplateService.getInstance();
  }

  async componentWillLoad() {
    await this.imageEventsHandler.init();
    await this.chartEventsHandler.init();

    const params = new URLSearchParams(window.location.search);
    this.loading = params.get('signin') === 'success';
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initDashboard();
    });

    await this.initDashboard();
  }

  private async initDashboard() {
    if (!authStore.state.authUser || authStore.state.anonymous) {
      return;
    }

    this.destroyListener();

    this.loading = true;

    try {
      const userDecks: Deck[] = await this.deckService.getUserDecks(authStore.state.authUser.uid);

      await this.templateService.init();

      this.decks = await this.fetchFirstSlides(userDecks);
      await this.filterDecks(null);

      // If some decks are currently cloned, we watch them to update GUI when clone has finished processing
      await this.initWatchForClonedDecks();
    } catch (err) {
      errorStore.state.error = 'Cannot init your dashboard.';
    }

    this.loading = false;
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
        if (deck?.deck?.data?.clone !== undefined) {
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

        await TemplateUtils.loadSlideTemplate(slide);

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

  private async filterDecksOnChange(e: CustomEvent) {
    if (e && e.detail) {
      await this.filterDecks(e.detail.value);
    } else {
      await this.filterDecks(null);
    }
  }

  private async blockSlide($event: CustomEvent) {
    await ($event?.target as HTMLDeckgoDeckElement).blockSlide(true);
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
    if (this.loading) {
      return this.renderLoading();
    }

    if (authStore.state.anonymous) {
      return this.renderAnonymousContent();
    }

    return this.renderGuardedContent();
  }

  private renderLoading() {
    return <app-spinner></app-spinner>;
  }

  private renderAnonymousContent() {
    return (
      <main class="ion-padding fit anonymous">
        <h1>{i18n.state.dashboard.welcome}</h1>

        {this.renderNotLoggedInText()}
        {this.renderCreateButton(true)}
      </main>
    );
  }

  private renderGuardedContent() {
    return <main class="ion-padding fit">{this.renderYourPresentations()}</main>;
  }

  private renderYourPresentations() {
    return (
      <Fragment>
        <h1>{i18n.state.dashboard.your_presentations}</h1>

        {this.renderDecksFilter()}

        {this.filteredDecks?.length > 0 ? undefined : this.renderCreateButton(false)}

        {this.renderDecks()}
      </Fragment>
    );
  }

  private renderNotLoggedInText() {
    return (
      <Fragment>
        <p>{renderI18n(i18n.state.dashboard.try, {placeholder: '{0}', value: <a onClick={() => signIn()}>{i18n.state.nav.sign_in.toLowerCase()}</a>})}</p>
        <p class="ion-no-margin">{i18n.state.core.free_open_source}</p>
      </Fragment>
    );
  }

  private renderDecksFilter() {
    if (this.filteredDecks?.length > 0) {
      return (
        <ion-searchbar
          debounce={500}
          animated={false}
          placeholder={i18n.state.dashboard.filter}
          onClick={($event) => $event.stopImmediatePropagation()}
          onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
          class="ion-no-padding ion-margin-top ion-margin-bottom"
        />
      );
    }

    return <p>{i18n.state.dashboard.no_slides}</p>;
  }

  private renderCreateButton(withSignIn: boolean) {
    return (
      <div class="toolbar-actions ion-margin-top">
        {withSignIn ? (
          <ion-button shape="round" color="light" onClick={() => signIn()} style={{'margin-right': '8px'}}>
            <ion-label>{i18n.state.nav.sign_in}</ion-label>
          </ion-button>
        ) : undefined}

        <app-start-deck></app-start-deck>
      </div>
    );
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
        <deckgo-deck
          embedded={true}
          keyboard={false}
          direction="horizontal"
          direction-mobile="horizontal"
          style={deck.style}
          onSlidesDidLoad={($event: CustomEvent) => this.blockSlide($event)}>
          {deck.slide}
          {deck.background}
          {deck.header}
          {deck.footer}
        </deckgo-deck>
      );
    }
  }
}
