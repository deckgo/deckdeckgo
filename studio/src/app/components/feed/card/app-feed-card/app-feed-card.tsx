import {Component, Prop, State, h, Element} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

import {Deck, DeckMetaAuthor} from '../../../../models/data/deck';

import {EnvironmentConfigService} from '../../../../services/core/environment/environment-config.service';

@Component({
  tag: 'app-feed-card',
  styleUrl: 'app-feed-card.scss',
  shadow: false,
})
export class AppFeedCard {
  @Element() el: HTMLElement;

  @Prop()
  compact: boolean = true;

  @Prop()
  deck: Deck;

  @State()
  private caption: string;

  @State()
  private description: string;

  @State()
  private author: string;

  @State()
  private authorPhotoUrl: string;

  @State()
  private formattedPublishedAt: string;

  @State()
  private tags: string[] = [];

  @State()
  private screenshot: string;

  @State()
  private width: number;

  async componentWillLoad() {
    await this.init();

    this.initWindowResize();
  }

  async componentDidLoad() {
    await this.onWindowResize();
  }

  componentDidUnload() {
    this.removeWindowResize();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private onWindowResize = async () => {
    this.width = this.el.offsetWidth;
  };

  private init(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.deck || !this.deck.data || !this.deck.data.meta) {
        resolve();
        return;
      }

      this.caption = this.deck.data.meta.title;
      this.description = this.deck.data.meta.description ? (this.deck.data.meta.description as string) : undefined;
      this.tags = this.deck.data.meta.tags as string[];

      this.author = this.deck.data.meta.author ? (this.deck.data.meta.author as DeckMetaAuthor).name : undefined;
      this.authorPhotoUrl =
        this.deck.data.meta.author && (this.deck.data.meta.author as DeckMetaAuthor).photo_url
          ? (this.deck.data.meta.author as DeckMetaAuthor).photo_url
          : undefined;

      await this.formatPublication();

      await this.initScreenshot();

      resolve();
    });
  }

  private formatPublication(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.deck.data.meta.published_at) {
        resolve();
        return;
      }

      const options: DateTimeFormatOptions = {year: 'numeric', month: 'short', day: 'numeric'};

      try {
        this.formattedPublishedAt = new Intl.DateTimeFormat('en-US', options).format(this.getDateObj(this.deck.data.meta.published_at));
      } catch (err) {
        this.formattedPublishedAt = undefined;
      }

      resolve();
    });
  }

  private initScreenshot(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.deck.data.meta.pathname || !this.deck.data.owner_id || this.deck.data.owner_id === undefined || this.deck.data.owner_id === '') {
        resolve();
        return;
      }

      const storageUrl: string = EnvironmentConfigService.getInstance().get('firebase').storageUrl;

      const path: string[] = this.deck.data.meta.pathname.split('/');

      if (!path || path.length < 3) {
        resolve();
        return;
      }

      // path[0] = ''
      // path[1] = username
      // path[2] = presentation name
      this.screenshot = `${storageUrl}${this.deck.data.owner_id}%2Fpresentations%2F${path[2]}%2Fdeckdeckgo.png?alt=media`;

      resolve();
    });
  }

  private getDateObj(myDate: any): Date {
    if (!myDate) {
      return null;
    }

    if (myDate instanceof String || typeof myDate === 'string') {
      return new Date('' + myDate);
    }

    // A Firebase Timestamp format
    if (
      myDate &&
      (myDate.seconds >= 0 || myDate.seconds < 0) &&
      (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0) &&
      typeof (myDate as any).toDate === 'function'
    ) {
      return new Date(myDate.toDate());
    }

    return myDate;
  }

  render() {
    return (
      <ion-card class="ion-margin-top" style={{'--card-width': `${this.width}px`}}>
        {this.renderCardContent()}
      </ion-card>
    );
  }

  private renderCardContent() {
    return (
      <ion-card-content class={this.compact ? 'ion-no-padding compact' : 'ion-no-padding'}>
        <deckgo-lazy-img img-src={this.screenshot} img-error-src="./assets/img/screenshot-not-found.png"></deckgo-lazy-img>

        <ion-card-header>
          <ion-card-title class="ion-text-wrap">{this.caption}</ion-card-title>

          {this.renderAuthor()}
        </ion-card-header>

        <p class="content ion-padding-start ion-padding-end ion-padding-bottom">{this.description}</p>

        {this.renderTags()}
      </ion-card-content>
    );
  }

  private renderAuthor() {
    if (this.author) {
      return (
        <p class="author">
          {this.renderAuthorAvatar()}
          <ion-label>
            {this.author} | {this.formattedPublishedAt}
          </ion-label>
        </p>
      );
    } else {
      return undefined;
    }
  }

  private renderAuthorAvatar() {
    if (this.authorPhotoUrl) {
      return <app-avatar src={this.authorPhotoUrl} aria-label={this.author}></app-avatar>;
    } else {
      return undefined;
    }
  }

  private renderTags() {
    if (this.tags && this.tags.length) {
      return <app-feed-card-tags tags={this.tags} class="ion-margin-start ion-margin-end ion-margin-bottom"></app-feed-card-tags>;
    } else {
      return undefined;
    }
  }
}
