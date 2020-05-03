import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

@Component({
  tag: 'app-demo',
  styleUrl: 'app-demo.scss',
})
export class AppDemo {
  @Element() el: HTMLElement;

  @State()
  private demoUrl: string;

  @Prop()
  selectedElement: HTMLElement;

  componentWillLoad() {
    if (this.selectedElement) {
      this.demoUrl = this.selectedElement.getAttribute('src');
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
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(this.demoUrl);
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.demoUrl = ($event.target as InputTargetEvent).value;
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="primary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Demo</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-item>
            <ion-input
              value={this.demoUrl}
              placeholder="Enter the URL of your app or website"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}></ion-input>
          </ion-item>
        </ion-list>

        <ion-button disabled={this.demoUrl === undefined || !this.demoUrl || this.demoUrl === ''} color="dark" shape="round" onClick={() => this.save()}>
          <ion-label>Save</ion-label>
        </ion-button>
      </ion-content>,
    ];
  }
}
