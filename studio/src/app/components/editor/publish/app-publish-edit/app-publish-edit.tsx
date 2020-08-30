import {Component, Event, EventEmitter, h, State} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import deckStore from '../../../../stores/deck.store';
import errorStore from '../../../../stores/error.store';
import feedStore from '../../../../stores/feed.store';
import publishStore from '../../../../stores/publish.store';
import apiUserStore from '../../../../stores/api.user.store';
import authStore from '../../../../stores/auth.store';

import {Deck} from '../../../../models/data/deck';

import {Resources} from '../../../../utils/core/resources';

import {DeckService} from '../../../../services/data/deck/deck.service';
import {PublishService} from '../../../../services/editor/publish/publish.service';

interface CustomInputEvent extends KeyboardEvent {
  data: string | null;
}

@Component({
  tag: 'app-publish-edit',
  styleUrl: 'app-publish-edit.scss',
})
export class AppPublishEdit {
  @State()
  private caption: string;

  @State()
  private description: string;

  @State()
  private valid: boolean = true;

  @State()
  private disablePublish: boolean = false;

  @State()
  private publishing: boolean = false;

  @State()
  private tag: string;

  @State()
  private tags: string[] = [];

  @State()
  private pushToGitHub: boolean = true;

  private deckService: DeckService;

  private readonly debounceUpdateDeck: () => void;

  @Event() private published: EventEmitter<string>;

  private publishService: PublishService;

  constructor() {
    this.deckService = DeckService.getInstance();

    this.publishService = PublishService.getInstance();

    this.debounceUpdateDeck = debounce(async () => {
      await this.updateDeck();
    }, 500);
  }

  async componentWillLoad() {
    await this.init();
  }

  componentDidLoad() {
    this.validateCaptionInput();
  }

  private async init() {
    if (!deckStore.state.deck || !deckStore.state.deck.data) {
      return;
    }

    this.caption = deckStore.state.deck.data.name;
    this.description =
      deckStore.state.deck.data.meta && deckStore.state.deck.data.meta.description
        ? (deckStore.state.deck.data.meta.description as string)
        : await this.getFirstSlideContent();
    this.tags = deckStore.state.deck.data.meta && deckStore.state.deck.data.meta.tags ? (deckStore.state.deck.data.meta.tags as string[]) : [];
    this.pushToGitHub = deckStore.state.deck.data.meta && deckStore.state.deck.data.meta.github !== undefined ? deckStore.state.deck.data.meta.github : true;
  }

  private getFirstSlideContent(): Promise<string> {
    return new Promise<string>(async (resolve) => {
      let content: string = '';

      if (!document) {
        resolve('');
        return;
      }

      const slide: HTMLElement = document.querySelector('deckgo-deck > *:first-child');

      if (slide && slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
        const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

        if (contentElement) {
          content = contentElement.textContent;
        }
      }

      resolve(content);
    });
  }

  private updateDeck(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.caption || this.caption === undefined || this.caption === '') {
        resolve();
        return;
      }

      this.disablePublish = true;

      try {
        if (!deckStore.state.deck || !deckStore.state.deck.data || !deckStore.state.deck.id) {
          this.disablePublish = false;
          resolve();
          return;
        }

        deckStore.state.deck.data.name = this.caption;

        const updatedDeck: Deck = await this.deckService.update(deckStore.state.deck);
        deckStore.state.deck = {...updatedDeck};

        this.disablePublish = false;
      } catch (err) {
        this.disablePublish = false;
        errorStore.state.error = err;
      }

      resolve();
    });
  }

  private async handleSubmit(e: Event) {
    e.preventDefault();

    await this.publish();
  }

  private publish(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        this.publishing = true;

        const publishedUrl: string = await this.publishService.publish(this.description, this.tags, this.pushToGitHub);

        this.published.emit(publishedUrl);

        this.publishing = false;

        // In case the user would have browse the feed before, reset it to fetch is updated or new presentation
        feedStore.reset();

        resolve();
      } catch (err) {
        this.publishing = false;
        errorStore.state.error = err;
        resolve();
      }
    });
  }

  private onCaptionInput($event: CustomEvent<KeyboardEvent>): Promise<void> {
    return new Promise<void>((resolve) => {
      let title: string = ($event.target as InputTargetEvent).value;
      if (title && title !== undefined && title !== '') {
        if (!this.validCaption(title)) {
          title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
        }
      }

      this.caption = title;

      this.debounceUpdateDeck();

      resolve();
    });
  }

  private validateCaptionInput() {
    this.valid = this.validCaption(this.caption);
  }

  private validCaption(title: string): boolean {
    if (!title || title === undefined || title == '' || title.length > Resources.Constants.DECK.TITLE_MAX_LENGTH) {
      return false;
    }

    const match: RegExpMatchArray | null = title.match(/[A-Za-z0-9\u00C0-\u00D6\u00D8-\u00f6\u00f8-\u00ff\s\-]+/g);

    if (!match || match.length <= 0 || match.length > 1) {
      return false;
    }

    return match[0] === title;
  }

  private onDescriptionInput($event: CustomEvent<KeyboardEvent>) {
    this.description = ($event.target as InputTargetEvent).value;
  }

  private validateDescriptionInput() {
    this.valid = this.validDescription();
  }

  private validDescription(): boolean {
    return (
      !this.description ||
      this.description === undefined ||
      this.description === '' ||
      this.description.length < Resources.Constants.DECK.DESCRIPTION_MAX_LENGTH
    );
  }

  private onTagInput($event: CustomEvent<KeyboardEvent>): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      if (($event.detail as CustomInputEvent).data === ' ' || ($event.detail as CustomInputEvent).data === ',') {
        this.addTag();
        resolve();
        return;
      }

      this.tag = ($event.target as InputTargetEvent).value;

      resolve();
    });
  }

  private onTagInputKeyUp($event: KeyboardEvent): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!$event) {
        resolve();
        return;
      }

      if ($event.code === 'Enter') {
        this.addTag();
      }

      resolve();
    });
  }

  private addTag() {
    if (this.tag && this.tag !== undefined && this.tag !== null && this.tag.length >= 3) {
      if (this.tag.charAt(0) === '#') {
        this.tag = this.tag.substr(1);
      }

      this.tag = this.tag.replace(' ', '');

      if (this.tags && this.tags.indexOf(this.tag) === -1) {
        this.tags = [...this.tags, this.tag.trim()];
        this.tag = null;
      }
    }
  }

  private removeTag($event: CustomEvent): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      const tag: string = $event.detail;

      if (!this.tags) {
        resolve();
        return;
      }

      const index: number = this.tags.findIndex((actualTag: string) => {
        return tag === actualTag;
      });

      if (index >= 0) {
        this.tags.splice(index, 1);
        this.tags = [...this.tags];
      }

      resolve();
    });
  }

  private async onGitHubChange($event: CustomEvent) {
    this.pushToGitHub = $event && $event.detail ? $event.detail.value : true;
  }

  render() {
    return (
      <article>
        <h1>Share your presentation online</h1>

        <p>Publish your presentation to share it with the world, your colleagues, friends and community.</p>

        <p>DeckDeckGo will distribute it online as a modern app.</p>

        <h2 class="ion-padding-top">Meta</h2>

        <p class="meta-text">
          Edit or review your presentation's title, summary and add or change tags (up to 5) to make your presentation more inviting to readers.
        </p>

        <form
          onSubmit={(e: Event) => this.handleSubmit(e)}
          onKeyPress={(e) => {
            e.key === 'Enter' && e.preventDefault();
          }}>
          <ion-list class="inputs-list">
            {this.renderTitleLabel()}

            <ion-item>
              <ion-input
                value={this.caption}
                debounce={500}
                minlength={3}
                disabled={this.publishing}
                maxlength={Resources.Constants.DECK.TITLE_MAX_LENGTH}
                required={true}
                input-mode="text"
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onCaptionInput(e)}
                onIonChange={() => this.validateCaptionInput()}></ion-input>
            </ion-item>

            <p class={`small ${this.valid ? undefined : 'error'}`}>
              The title could be provided with latin characters, arabic numerals, spaces and dash. It must not be longer than{' '}
              {Resources.Constants.DECK.TITLE_MAX_LENGTH} characters.
            </p>

            <ion-item class="item-title">
              <ion-label>Description</ion-label>
            </ion-item>

            <ion-item>
              <ion-textarea
                rows={5}
                value={this.description}
                debounce={500}
                disabled={this.publishing}
                maxlength={Resources.Constants.DECK.DESCRIPTION_MAX_LENGTH}
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onDescriptionInput(e)}
                onIonChange={() => this.validateDescriptionInput()}></ion-textarea>
            </ion-item>

            <ion-item class="item-title ion-margin-top">
              <ion-label>Tags</ion-label>
            </ion-item>

            <ion-item>
              <ion-input
                debounce={500}
                input-mode="text"
                value={this.tag}
                placeholder="Add a tag..."
                disabled={!this.tags || this.tags.length >= 5 || this.publishing}
                onKeyUp={($event: KeyboardEvent) => this.onTagInputKeyUp($event)}
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onTagInput(e)}></ion-input>
            </ion-item>

            <app-feed-card-tags
              tags={this.tags}
              editable={true}
              disable-remove={this.publishing}
              onRemoveTag={($event: CustomEvent) => this.removeTag($event)}></app-feed-card-tags>
          </ion-list>

          {this.renderGitHub()}

          <div class="ion-padding ion-text-center publish">{this.renderPublish()}</div>
        </form>

        <p class="small">DeckDeckGo will automatically generate the social card for your presentation based on the first slide of your deck.</p>
      </article>
    );
  }

  private renderTitleLabel() {
    return (
      <ion-item class={`item-title ${this.valid ? undefined : 'error'}`}>
        <ion-label>
          Title {this.valid ? undefined : <ion-icon aria-label="Title needs to match the expected format" name="warning-outline"></ion-icon>}
        </ion-label>
      </ion-item>
    );
  }

  private renderPublish() {
    if (!this.publishing) {
      return (
        <ion-button type="submit" disabled={!this.valid || this.disablePublish || !apiUserStore.state.apiUser} color="tertiary" shape="round">
          <ion-label>Publish now</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="publishing">
          <ion-progress-bar value={publishStore.state.progress} color="tertiary"></ion-progress-bar>
          <ion-label>Hang on, we are publishing your presentation</ion-label>
        </div>
      );
    }
  }

  private renderGitHub() {
    if (!authStore.state.gitHub) {
      return undefined;
    }

    return [
      <h2 class="ion-padding-top">
        GitHub <ion-icon name="logo-github" aria-label="GitHub"></ion-icon>
      </h2>,
      <p class="meta-text">Push the source code of the presentation to a public GitHub repo?</p>,
      <ion-list class="inputs-list ion-margin-bottom">
        <ion-radio-group value={this.pushToGitHub} onIonChange={($event) => this.onGitHubChange($event)}>
          <ion-item>
            <ion-radio value={true} mode="md" disabled={this.publishing}></ion-radio>
            <ion-label>Yes</ion-label>
          </ion-item>

          <ion-item>
            <ion-radio value={false} mode="md" disabled={this.publishing}></ion-radio>
            <ion-label>No</ion-label>
          </ion-item>
        </ion-radio-group>
      </ion-list>,
    ];
  }
}
