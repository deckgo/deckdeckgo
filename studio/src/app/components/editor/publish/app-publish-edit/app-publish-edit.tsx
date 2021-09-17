import {Component, Event, EventEmitter, h, State} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {isSlide} from '@deckdeckgo/deck-utils';

import {Deck} from '@deckdeckgo/editor';

import deckStore from '../../../../stores/deck.store';
import errorStore from '../../../../stores/error.store';
import authStore from '../../../../stores/auth.store';
import i18n from '../../../../stores/i18n.store';

import {Constants} from '../../../../types/core/constants';

import {DeckFirebaseProvider} from '../../../../providers/data/deck/deck.firebase.provider';
import {PublishService} from '../../../../services/editor/publish/publish.service';

import {getPublishedUrl} from '../../../../utils/core/share.utils';
import {renderI18n} from '../../../../utils/core/i18n.utils';

import {AppIcon} from '../../../core/app-icon/app-icon';

interface CustomInputEvent extends KeyboardEvent {
  data: string | null;
}

@Component({
  tag: 'app-publish-edit',
  styleUrl: 'app-publish-edit.scss'
})
export class AppPublishEdit {
  @State()
  private caption: string;

  @State()
  private description: string;

  @State()
  private validTitle: boolean = true;

  @State()
  private validDescription: boolean = true;

  @State()
  private disablePublish: boolean = false;

  @State()
  private publishing: boolean = false;

  @State()
  private progress: number | undefined = undefined;

  @State()
  private tag: string;

  @State()
  private tags: string[] = [];

  @State()
  private pushToGitHub: boolean = true;

  private deckFirebaseProvider: DeckFirebaseProvider;

  private readonly debounceUpdateDeck: () => void;

  @Event() private published: EventEmitter<string>;

  private publishService: PublishService;

  private destroyDeckListener;

  constructor() {
    this.deckFirebaseProvider = DeckFirebaseProvider.getInstance();

    this.publishService = PublishService.getInstance();

    this.debounceUpdateDeck = debounce(async () => {
      await this.updateDeck();
    }, 500);
  }

  async componentWillLoad() {
    await this.init();

    this.destroyDeckListener = deckStore.onChange('deck', async (deck: Deck | undefined) => {
      // Deck is maybe updating while we have set it to true manually
      this.publishing = this.publishing || deck.data.deploy?.api?.status === 'scheduled';
    });
  }

  componentDidLoad() {
    this.validateCaptionInput();
  }

  disconnectedCallback() {
    if (this.destroyDeckListener) {
      this.destroyDeckListener();
    }
  }

  private async init() {
    if (!deckStore.state.deck || !deckStore.state.deck.data) {
      return;
    }

    this.caption = deckStore.state.deck.data.name;
    this.description = deckStore.state.deck.data.meta?.description
      ? (deckStore.state.deck.data.meta.description as string)
      : await this.getFirstSlideContent();
    this.tags = deckStore.state.deck.data.meta?.tags ? (deckStore.state.deck.data.meta.tags as string[]) : [];
    this.pushToGitHub = deckStore.state.deck.data.github ? deckStore.state.deck.data.github.publish : true;
  }

  private getFirstSlideContent(): Promise<string> {
    return new Promise<string>(async (resolve) => {
      let content: string = '';

      if (!document) {
        resolve('');
        return;
      }

      const slide: HTMLElement = document.querySelector('app-editor main deckgo-deck > *:first-child');

      if (isSlide(slide)) {
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

        const updatedDeck: Deck = await this.deckFirebaseProvider.update(deckStore.state.deck);
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

        this.onSuccessfulPublish();

        await this.publishService.publish(this.description, this.tags, this.pushToGitHub);

        resolve();
      } catch (err) {
        this.publishing = false;
        errorStore.state.error = err;
        resolve();
      }
    });
  }

  private onSuccessfulPublish() {
    const currentDeck: Deck = {...deckStore.state.deck};

    const destroyDeckDeployListener = deckStore.onChange('deck', async (deck: Deck | undefined) => {
      if (deck?.data?.deploy?.api?.status === 'successful') {
        destroyDeckDeployListener();

        await this.delayNavigation(currentDeck.data.api_id !== deckStore.state.deck.data.api_id);
      }
    });
  }

  // Even if we fixed the delay to publish to Cloudfare CDN (#195), sometimes if too quick, the presentation will not be correctly published
  // Therefore, to avoid such problem, we add a bit of delay in the process but only for the first publish
  private async delayNavigation(newApiId: boolean) {
    this.progress = 0;

    const interval = setInterval(
      () => {
        this.progress += 0.1;
      },
      newApiId ? 7000 / 10 : 700 / 10
    );

    setTimeout(
      async () => {
        if (interval) {
          clearInterval(interval);
        }

        this.progress = 1;

        // Just for display so the progress bar reaches 100% for the eyes
        setTimeout(async () => {
          const publishedUrl: string = await getPublishedUrl(deckStore.state.deck);
          this.published.emit(publishedUrl);
        }, 200);
      },
      newApiId ? 7000 : 700
    );
  }

  private onCaptionInput($event: CustomEvent<KeyboardEvent>): Promise<void> {
    return new Promise<void>((resolve) => {
      let title: string = ($event.target as InputTargetEvent).value;
      if (title && title !== undefined && title !== '') {
        if (!this.validCaption(title)) {
          title = title.substr(0, Constants.DECK.TITLE_MAX_LENGTH);
        }
      }

      this.caption = title;

      this.debounceUpdateDeck();

      resolve();
    });
  }

  private validateCaptionInput() {
    this.validTitle = this.validCaption(this.caption);
  }

  private validCaption(title: string): boolean {
    if (!title || title === undefined || title == '' || title.length > Constants.DECK.TITLE_MAX_LENGTH) {
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
    this.validDescription =
      !this.description ||
      this.description === undefined ||
      this.description === '' ||
      this.description.length <= Constants.DECK.DESCRIPTION_MAX_LENGTH;
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
    const disable: boolean = this.publishing || this.progress !== undefined;

    return (
      <article>
        <h1>{i18n.state.publish_edit.share}</h1>

        <p>{i18n.state.publish_edit.publish}</p>

        <p>{i18n.state.publish_edit.modern_app}</p>

        <h2 class="ion-padding-top">{i18n.state.publish_edit.meta}</h2>

        <p class="meta-text">{i18n.state.publish_edit.title_edit}</p>

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
                disabled={disable}
                maxlength={Constants.DECK.TITLE_MAX_LENGTH}
                required={true}
                input-mode="text"
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onCaptionInput(e)}
                onIonChange={() => this.validateCaptionInput()}></ion-input>
            </ion-item>

            <p class={`small ${this.validTitle ? undefined : 'error'}`}>{i18n.state.publish_edit.title_max_chars}</p>

            <ion-item class="item-title">
              <ion-label>{i18n.state.publish_edit.description}</ion-label>
            </ion-item>

            <ion-item>
              <ion-textarea
                rows={5}
                value={this.description}
                debounce={500}
                disabled={disable}
                maxlength={Constants.DECK.DESCRIPTION_MAX_LENGTH}
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onDescriptionInput(e)}
                onIonChange={() => this.validateDescriptionInput()}></ion-textarea>
            </ion-item>

            <ion-item class="item-title ion-margin-top">
              <ion-label>{i18n.state.publish_edit.tags}</ion-label>
            </ion-item>

            <ion-item>
              <ion-input
                debounce={500}
                input-mode="text"
                value={this.tag}
                placeholder="Add a tag..."
                disabled={!this.tags || this.tags.length >= 5 || disable}
                onKeyUp={($event: KeyboardEvent) => this.onTagInputKeyUp($event)}
                onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onTagInput(e)}></ion-input>
            </ion-item>

            <app-publish-tags
              tags={this.tags}
              disable-remove={disable}
              onRemoveTag={($event: CustomEvent) => this.removeTag($event)}></app-publish-tags>
          </ion-list>

          {this.renderGitHub(disable)}

          <div class="ion-padding ion-text-center publish">{this.renderPublish(disable)}</div>
        </form>

        <p class="small">{i18n.state.publish_edit.social_card}</p>

        {this.renderFailure()}
      </article>
    );
  }

  private renderFailure() {
    if (deckStore.state.deck?.data?.deploy?.api?.status !== 'failure') {
      return undefined;
    }

    return (
      <p class="small error ion-margin-top">
        <AppIcon name="warning" ariaLabel="" ariaHidden={true}></AppIcon>{' '}
        {renderI18n(i18n.state.publish_edit.error_previous, {
          placeholder: '{0}',
          value: (
            <a href="https://deckdeckgo.com/en/contact/" rel="noopener norefferer" target="_blank">
              {i18n.state.publish_edit.contact}
            </a>
          )
        })}
      </p>
    );
  }

  private renderTitleLabel() {
    return (
      <ion-item class={`item-title ${this.validTitle ? undefined : 'error'}`}>
        <ion-label>
          {i18n.state.publish_edit.title}{' '}
          {this.validTitle ? undefined : <AppIcon name="warning" ariaLabel="Title needs to match the expected format"></AppIcon>}
        </ion-label>
      </ion-item>
    );
  }

  private renderPublish(disable: boolean) {
    if (!disable) {
      return (
        <ion-button
          type="submit"
          disabled={!this.validTitle || !this.validDescription || this.disablePublish}
          color="tertiary"
          shape="round">
          <ion-label>{i18n.state.publish_edit.publish_now}</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="publishing">
          {this.renderProgressBar()}
          <ion-label>{i18n.state.publish_edit.hang_on_publishing}</ion-label>
        </div>
      );
    }
  }

  private renderProgressBar() {
    if (this.progress === undefined) {
      return <ion-progress-bar type="indeterminate" color="tertiary"></ion-progress-bar>;
    }

    return <ion-progress-bar value={this.progress} color="tertiary"></ion-progress-bar>;
  }

  private renderGitHub(disable: boolean) {
    if (!authStore.state.gitHub) {
      return undefined;
    }

    return [
      <h2 class="ion-padding-top">
        GitHub <AppIcon name="github" ariaLabel="" ariaHidden={true}></AppIcon>
      </h2>,
      this.renderGitHubText(),
      <ion-list class="inputs-list ion-margin-bottom">
        <ion-radio-group value={this.pushToGitHub} onIonChange={($event) => this.onGitHubChange($event)} class="inline">
          <ion-item>
            <ion-radio value={true} mode="md" disabled={disable}></ion-radio>
            <ion-label>{i18n.state.core.yes}</ion-label>
          </ion-item>

          <ion-item>
            <ion-radio value={false} mode="md" disabled={disable}></ion-radio>
            <ion-label>{i18n.state.core.no}</ion-label>
          </ion-item>
        </ion-radio-group>
      </ion-list>
    ];
  }

  private renderGitHubText() {
    if (!deckStore.state.deck || !deckStore.state.deck.data || !deckStore.state.deck.data.github) {
      return <p class="meta-text">{i18n.state.publish_edit.source_push}</p>;
    }

    return <p class="meta-text">{i18n.state.publish_edit.source_submit}</p>;
  }
}
