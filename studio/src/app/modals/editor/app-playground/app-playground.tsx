import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

@Component({
  tag: 'app-playground',
  styleUrl: 'app-playground.scss',
})
export class AppPlayground {
  @Element() el: HTMLElement;

  @State()
  private playgroundSrc: string;

  @Prop()
  selectedElement: HTMLElement;

  componentWillLoad() {
    if (this.selectedElement) {
      this.playgroundSrc = this.selectedElement.getAttribute('src');
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
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(this.playgroundSrc);
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.playgroundSrc = ($event.target as InputTargetEvent).value;
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

        <ion-button
          disabled={this.playgroundSrc === undefined || !this.playgroundSrc || this.playgroundSrc === ''}
          color="dark"
          shape="round"
          onClick={() => this.save()}>
          <ion-label>Save</ion-label>
        </ion-button>

        <p>Embed your best Codepen, JSFiddle or WebComponents.dev. All you need is to enter their full links. Check out this YouTube video for a tutorial.</p>
      </ion-content>,
    ];
  }
}
