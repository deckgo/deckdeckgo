import {DeckdeckgoPlaygroundTheme} from '@deckdeckgo/slide-playground';
import {Component, Element, h, Listen, Prop, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import i18n from '../../../stores/i18n.store';
import {PlaygroundAction} from '../../../types/editor/playground-action';
import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-playground',
  styleUrl: 'app-playground.scss'
})
export class AppPlayground {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @State()
  private playgroundSrc: string;

  @State()
  private playgroundTheme: DeckdeckgoPlaygroundTheme = DeckdeckgoPlaygroundTheme.DEFAULT;

  @State()
  private supportsTheme: boolean = true;

  componentWillLoad() {
    if (this.selectedTarget) {
      this.playgroundSrc = this.selectedTarget.getAttribute('src');

      this.playgroundTheme =
        this.selectedTarget && this.selectedTarget.hasAttribute('theme')
          ? (this.selectedTarget.getAttribute('theme') as DeckdeckgoPlaygroundTheme)
          : DeckdeckgoPlaygroundTheme.DEFAULT;

      this.initSupportsTheme();
    }
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  async save() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss({
      src: this.playgroundSrc,
      theme: this.supportsTheme ? this.playgroundTheme : undefined
    } as PlaygroundAction);
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.playgroundSrc = ($event.target as InputTargetEvent).value;

    this.initSupportsTheme();
  }

  private initSupportsTheme() {
    this.supportsTheme = this.playgroundSrc !== undefined && !this.playgroundSrc.match(/webcomponents\.[\s\S]*/);
  }

  private async toggleTheme($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.playgroundTheme = $event.detail.value;
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="medium">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.templates.playground}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-item>
            <ion-input
              value={this.playgroundSrc}
              placeholder={i18n.state.editor.url_playground}
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}></ion-input>
          </ion-item>
        </ion-list>

        <div class="theme">
          <ion-select
            value={this.playgroundTheme}
            disabled={!this.supportsTheme}
            placeholder={i18n.state.editor.select_theme}
            onIonChange={($event: CustomEvent) => this.toggleTheme($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            {Object.keys(DeckdeckgoPlaygroundTheme).map((key: string) => {
              return (
                <ion-select-option value={DeckdeckgoPlaygroundTheme[key]}>
                  {DeckdeckgoPlaygroundTheme[key].replace(/^\w/, (c) => c.toUpperCase())} theme
                </ion-select-option>
              );
            })}
          </ion-select>
        </div>

        <ion-button
          disabled={this.playgroundSrc === undefined || !this.playgroundSrc || this.playgroundSrc === ''}
          color="dark"
          shape="round"
          class="ion-margin-top"
          onClick={() => this.save()}>
          <ion-label>{i18n.state.core.save}</ion-label>
        </ion-button>

        <p>
          {renderI18n(i18n.state.editor.embed_codepen, {
            placeholder: '{0}',
            value: (
              <a href="https://www.youtube.com/watch?v=nS4A2XSwQrw" target="_blank" aria-label="Demo on YouTube" rel="noopener noreferrer">
                {i18n.state.editor.youtube_video}
              </a>
            )
          })}
        </p>
      </ion-content>
    ];
  }
}
