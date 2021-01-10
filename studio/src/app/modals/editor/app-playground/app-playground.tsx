import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

import {DeckdeckgoPlaygroundTheme} from '@deckdeckgo/slide-playground';
import {PlaygroundAction} from '../../../types/editor/playground-action';

@Component({
  tag: 'app-playground',
  styleUrl: 'app-playground.scss',
})
export class AppPlayground {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private playgroundSrc: string;

  @State()
  private playgroundTheme: DeckdeckgoPlaygroundTheme = DeckdeckgoPlaygroundTheme.DEFAULT;

  @State()
  private supportsTheme: boolean = true;

  componentWillLoad() {
    if (this.selectedElement) {
      this.playgroundSrc = this.selectedElement.getAttribute('src');

      this.playgroundTheme =
        this.selectedElement && this.selectedElement.hasAttribute('theme')
          ? (this.selectedElement.getAttribute('theme') as DeckdeckgoPlaygroundTheme)
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
      theme: this.supportsTheme ? this.playgroundTheme : undefined,
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
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Playground</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-item>
            <ion-input
              value={this.playgroundSrc}
              placeholder="Enter the URL of the playground"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}></ion-input>
          </ion-item>
        </ion-list>

        <div class="theme">
          <ion-select
            value={this.playgroundTheme}
            disabled={!this.supportsTheme}
            placeholder="Select a theme"
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
          <ion-label>Save</ion-label>
        </ion-button>

        <p>
          Embed your best CodePen, JSFiddle or WebComponents.dev. All you need is to enter their full links. Check out this{' '}
          <a href="https://www.youtube.com/watch?v=nS4A2XSwQrw" target="_blank" aria-label="Demo on YouTube" rel="noopener noreferrer">
            YouTube video
          </a>{' '}
          for a quick tutorial.
        </p>
      </ion-content>,
    ];
  }
}
