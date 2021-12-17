import {Component, Event, EventEmitter, h, State} from '@stencil/core';

import {isSlide} from '@deckdeckgo/deck-utils';

import {Deck, deckSelector} from '@deckdeckgo/editor';

import editorStore from '../../../../../stores/editor.store';
import errorStore from '../../../../../stores/error.store';
import authStore from '../../../../../stores/auth.store';
import i18n from '../../../../../stores/i18n.store';

import {Constants} from '../../../../../types/core/constants';

import {publish, publishUrl} from '../../../../../providers/publish/publish.provider';

import {renderI18n} from '../../../../../utils/core/i18n.utils';

import {AppIcon} from '../../../../core/app-icon/app-icon';
import {firebase} from '../../../../../utils/core/environment.utils';

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
  private publishing: boolean = false;

  @State()
  private progress: number | undefined = undefined;

  @State()
  private tag: string;

  @State()
  private tags: string[] = [];

  @State()
  private pushToGitHub: boolean = true;

  @Event() private published: EventEmitter<string>;

  private firebaseEnabled: boolean = firebase();

  private destroyDeckListener;

  componentWillLoad() {
    this.init();

    this.destroyDeckListener = editorStore.onChange('deck', async (deck: Deck | undefined) => {
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

  private init() {
    if (!editorStore.state.deck || !editorStore.state.deck.data) {
      return;
    }

    this.caption = editorStore.state.deck.data.name;
    this.description = editorStore.state.deck.data.meta?.description
      ? editorStore.state.deck.data.meta.description
      : this.getFirstSlideContent();
    this.tags = editorStore.state.deck.data.meta?.tags ? (editorStore.state.deck.data.meta.tags as string[]) : [];
    this.pushToGitHub = editorStore.state.deck.data.github ? editorStore.state.deck.data.github.publish : true;
  }

  private getFirstSlideContent(): string | undefined {
    const slide: HTMLElement = document.querySelector(`${deckSelector} > *:first-child`);

    if (isSlide(slide)) {
      const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

      if (contentElement) {
        return contentElement.textContent;
      }
    }

    return undefined;
  }

  private async handleSubmit($event: FormDataEvent) {
    $event.preventDefault();

    await this.publishDeck();
  }

  private publishDeck(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        this.publishing = true;

        this.onSuccessfulPublish();

        await publish({name: this.caption, description: this.description, tags: this.tags, github: this.pushToGitHub});

        resolve();
      } catch (err) {
        this.publishing = false;
        errorStore.state.error = err;
        resolve();
      }
    });
  }

  private onSuccessfulPublish() {
    const currentDeck: Deck = {...editorStore.state.deck};

    const destroyDeckDeployListener = editorStore.onChange('deck', async (deck: Deck | undefined) => {
      if (deck?.data?.deploy?.api?.status === 'successful') {
        destroyDeckDeployListener();

        await this.delayNavigation(currentDeck.data.api_id !== editorStore.state.deck.data.api_id);
      }
    });
  }

  // Even if we fixed the delay to publish to Cloudflare CDN (#195), sometimes if too quick, the presentation will not be correctly published
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
          const publishedUrl: string = await publishUrl(editorStore.state.deck);
          this.published.emit(publishedUrl);
        }, 200);
      },
      newApiId ? 7000 : 700
    );
  }

  private onCaptionInput($event: CustomEvent<KeyboardEvent>) {
    let title: string = ($event.target as InputTargetEvent).value;
    if (title && title !== undefined && title !== '') {
      if (!this.validCaption(title)) {
        title = title.substr(0, Constants.DECK.TITLE_MAX_LENGTH);
      }
    }

    this.caption = title;
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

  private onTagInput($event: CustomEvent<KeyboardEvent>) {
    if (!$event || !$event.detail) {
      return;
    }

    if (($event.detail as CustomInputEvent).data === ' ' || ($event.detail as CustomInputEvent).data === ',') {
      this.addTag();
      return;
    }

    this.tag = ($event.target as InputTargetEvent).value;
  }

  private onTagInputKeyUp($event: KeyboardEvent): Promise<void> {
    if (!$event) {
      return;
    }

    if ($event.code === 'Enter') {
      this.addTag();
    }
  }

  private addTag() {
    if (this.tag && this.tag !== undefined && this.tag !== null && this.tag.length >= 3) {
      if (this.tag.charAt(0) === '#') {
        this.tag = this.tag.slice(1);
      }

      this.tag = this.tag.replace(' ', '');

      if (this.tags && this.tags.indexOf(this.tag) === -1) {
        this.tags = [...this.tags, this.tag.trim()];
        this.tag = null;
      }
    }
  }

  private removeTag($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const tag: string = $event.detail;

    if (!this.tags) {
      return;
    }

    const index: number = this.tags.findIndex((actualTag: string) => {
      return tag === actualTag;
    });

    if (index >= 0) {
      this.tags.splice(index, 1);
      this.tags = [...this.tags];
    }
  }

  private onGitHubChange($event: CustomEvent) {
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
          onSubmit={async ($event: FormDataEvent) => await this.handleSubmit($event)}
          onKeyPress={($event: KeyboardEvent) => {
            $event.key === 'Enter' && $event.preventDefault();
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
                onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCaptionInput($event)}
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
                onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onTagInput($event)}></ion-input>
            </ion-item>

            <app-publish-tags
              tags={this.tags}
              disable-remove={disable}
              onRemoveTag={($event: CustomEvent) => this.removeTag($event)}></app-publish-tags>
          </ion-list>

          {this.renderGitHub(disable)}

          <div class="ion-padding ion-text-center publish">{this.renderPublish(disable)}</div>
        </form>

        <p class="small">{this.firebaseEnabled && i18n.state.publish_edit.social_card}</p>

        {this.renderFailure()}
      </article>
    );
  }

  private renderFailure() {
    if (editorStore.state.deck?.data?.deploy?.api?.status !== 'failure') {
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
        <ion-button type="submit" disabled={!this.validTitle || !this.validDescription} color="tertiary" shape="round">
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
    if (!editorStore.state.deck || !editorStore.state.deck.data || !editorStore.state.deck.data.github) {
      return <p class="meta-text">{i18n.state.publish_edit.source_push}</p>;
    }

    return <p class="meta-text">{i18n.state.publish_edit.source_submit}</p>;
  }
}
