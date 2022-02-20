import {isSlide} from '@deckdeckgo/deck-utils';
import {Deck, deckSelector, Doc, Meta} from '@deckdeckgo/editor';
import {errorStore, editorStore} from '@deckdeckgo/studio';
import {Component, Event, EventEmitter, Fragment, h, State} from '@stencil/core';
import {Constants} from '../../../../../config/constants';
import {EnvironmentDeckDeckGoConfig} from '../../../../../config/environment-config';
import {publish, publishUrl} from '../../../../../providers/publish/publish.provider';
import {EnvironmentConfigService} from '../../../../../services/environment/environment-config.service';
import authStore from '../../../../../stores/auth.store';
import i18n from '../../../../../stores/i18n.store';
import {firebase} from '../../../../../utils/core/environment.utils';
import {renderI18n} from '../../../../../utils/core/i18n.utils';
import {AppIcon} from '../../../../core/app-icon/app-icon';

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
  private canonical: string;

  @State()
  private validCanonical: boolean = true;

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

  private destroyListener;

  private mode: 'doc' | 'deck' = editorStore.default.state.doc !== null ? 'doc' : 'deck';

  componentWillLoad() {
    this.init();

    // Firebase only
    this.destroyListener = editorStore.default.onChange('deck', async (deck: Deck | undefined) => {
      // Deck is maybe updating while we have set it to true manually
      this.publishing = this.publishing || deck?.data.deploy?.api?.status === 'scheduled';
    });
  }

  componentDidLoad() {
    this.validateCaptionInput();
  }

  disconnectedCallback() {
    this.destroyListener?.();
  }

  private init() {
    if (this.mode === 'doc') {
      const {data} = editorStore.default.state.doc;
      const {meta} = data;

      this.initInputs({name: meta?.title || data.name, description: meta?.description || '', tags: meta?.tags || []});

      this.canonical = meta?.canonical ?? '';

      return;
    }

    const {data} = editorStore.default.state.deck;
    const {meta} = data;

    this.initInputs({
      name: meta?.title || data.name,
      description: meta?.description || this.getFirstSlideContent(),
      tags: meta?.tags || []
    });

    this.pushToGitHub = editorStore.default.state.deck.data.github ? editorStore.default.state.deck.data.github.publish : true;
  }

  private initInputs({name, description, tags}: {name: string; description: string; tags: string[]}) {
    this.caption = name;
    this.description = description;
    this.tags = tags;
  }

  private getFirstSlideContent(): string | '' {
    const slide: HTMLElement = document.querySelector(`${deckSelector} > *:first-child`);

    if (isSlide(slide)) {
      const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

      if (contentElement) {
        return contentElement.textContent;
      }
    }

    return '';
  }

  private async handleSubmit($event: UIEvent) {
    $event.preventDefault();

    await this.publish();
  }

  private publish(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        this.publishing = true;

        this.onSuccessfulPublish();

        await publish({
          name: this.caption,
          description: this.description,
          tags: this.tags,
          github: this.pushToGitHub,
          canonical: this.canonical
        });

        resolve();
      } catch (err) {
        this.publishing = false;
        errorStore.default.state.error = err;
        resolve();
      }
    });
  }

  private onSuccessfulPublish() {
    if (this.mode === 'doc') {
      const destroyListener = editorStore.default.onChange('doc', async (doc: Doc | undefined) => {
        if (doc?.data?.deploy?.api?.status === 'successful') {
          destroyListener();

          await this.navigate({meta: editorStore.default.state.doc.data.meta});
        }
      });

      return;
    }

    const currentDeck: Deck = {...editorStore.default.state.deck};

    const destroyListener = editorStore.default.onChange('deck', async (deck: Deck | undefined) => {
      if (deck?.data?.deploy?.api?.status === 'successful') {
        destroyListener();

        // Even if we fixed the delay to publish to Cloudflare CDN and Firebase / AWS deployment (#195), sometimes if too quick, the presentation will not be correctly published
        // Therefore, to avoid such problem, we add a bit of delay in the process but only for the first publish
        await this.navigate({
          delay: currentDeck.data.api_id !== editorStore.default.state.deck.data.api_id,
          meta: editorStore.default.state.deck.data.meta
        });
      }
    });
  }

  private async navigate({delay = false, meta}: {delay?: boolean; meta: Meta}) {
    this.progress = 0;

    const interval = setInterval(
      () => {
        this.progress += 0.1;
      },
      delay ? 7000 / 10 : 700 / 10
    );

    setTimeout(
      async () => {
        if (interval) {
          clearInterval(interval);
        }

        this.progress = 1;

        // Just for display so the progress bar reaches 100% for the eyes
        setTimeout(async () => {
          const publishedUrl: string = await publishUrl(meta);
          this.published.emit(publishedUrl);
        }, 200);
      },
      delay ? 7000 : 700
    );
  }

  private onCaptionInput($event: CustomEvent<KeyboardEvent>) {
    let title: string = ($event.target as InputTargetEvent).value;
    if (title && title !== undefined && title !== '') {
      if (!this.validCaption(title)) {
        title = title.slice(0, this.titleMaxLength());
      }
    }

    this.caption = title;
  }

  private titleMaxLength(): number {
    return this.mode === 'doc' ? Constants.DOC.TITLE_MAX_LENGTH : Constants.DECK.TITLE_MAX_LENGTH;
  }

  private validateCaptionInput() {
    this.validTitle = this.validCaption(this.caption);
  }

  private validCaption(title: string): boolean {
    if (!title || title === undefined || title == '' || title.length > this.titleMaxLength()) {
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

  private onCanonicalInput($event: CustomEvent<KeyboardEvent>) {
    this.canonical = ($event.target as InputTargetEvent).value;
  }

  private validateDescriptionInput() {
    this.validDescription =
      !this.description ||
      this.description === undefined ||
      this.description === '' ||
      this.description.length <= Constants.DECK.DESCRIPTION_MAX_LENGTH;
  }

  private validateCanonicalInput() {
    try {
      const url: URL = new URL(this.canonical);

      this.validCanonical = url.protocol === 'https:';
    } catch (err) {
      this.validCanonical = false;
    }
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
          onSubmit={async ($event: UIEvent) => await this.handleSubmit($event)}
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
                maxlength={this.titleMaxLength()}
                required={true}
                input-mode="text"
                onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCaptionInput($event)}
                onIonChange={() => this.validateCaptionInput()}></ion-input>
            </ion-item>

            <p class={`small ${this.validTitle ? undefined : 'error'}`}>
              {renderI18n(i18n.state.publish_edit.title_max_chars, {
                placeholder: '{0}',
                value: `${this.titleMaxLength()}`
              })}
            </p>

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

            {this.renderCanonical(disable)}

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

        {this.renderSocialImage()}
      </article>
    );
  }

  private renderFailure() {
    if (editorStore.default.state.deck?.data?.deploy?.api?.status !== 'failure') {
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

  private renderCanonical(disable: boolean) {
    if (this.mode !== 'doc') {
      return undefined;
    }

    return (
      <Fragment>
        <ion-item class={`item-title ${this.validCanonical ? undefined : 'error'}`}>
          <ion-label>
            Canonical URL {this.validCanonical ? undefined : <AppIcon name="warning" ariaLabel="Invalid canonical url"></AppIcon>}
          </ion-label>
        </ion-item>

        <ion-item>
          <ion-input
            value={this.canonical}
            debounce={500}
            disabled={disable}
            required={false}
            input-mode="text"
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.onCanonicalInput($event)}
            onIonChange={() => this.validateCanonicalInput()}></ion-input>
        </ion-item>
      </Fragment>
    );
  }

  private renderPublish(disable: boolean) {
    if (!disable) {
      return (
        <ion-button
          type="submit"
          disabled={!this.validTitle || !this.validDescription || !this.validCanonical}
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
    if (!editorStore.default.state.deck || !editorStore.default.state.deck.data || !editorStore.default.state.deck.data.github) {
      return <p class="meta-text">{i18n.state.publish_edit.source_push}</p>;
    }

    return <p class="meta-text">{i18n.state.publish_edit.source_submit}</p>;
  }

  private renderSocialImage() {
    return (
      <deckgo-social-img
        text={this.caption}
        imgSrc={`${
          EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').globalAssetsUrl
        }/img/deckdeckgo-logo.svg`}></deckgo-social-img>
    );
  }
}
