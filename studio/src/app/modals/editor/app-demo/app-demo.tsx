import {Component, Element, h, Listen, Prop, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import i18n from '../../../stores/i18n.store';
import {DemoAction} from '../../../types/editor/demo-action';

@Component({
  tag: 'app-demo',
  styleUrl: 'app-demo.scss'
})
export class AppDemo {
  @Element() el: HTMLElement;

  @State()
  private demoSrc: string;

  @State()
  private demoMode: 'md' | 'ios' = 'md';

  @Prop()
  selectedTarget: HTMLElement;

  componentWillLoad() {
    if (this.selectedTarget) {
      const demo: HTMLElement = this.selectedTarget.querySelector('deckgo-demo');

      if (demo) {
        this.demoSrc = demo.getAttribute('src');
        this.demoMode = demo.getAttribute('mode') as 'md' | 'ios';
      }
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
      src: this.demoSrc,
      mode: this.demoMode
    } as DemoAction);
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.demoSrc = ($event.target as InputTargetEvent).value;
  }

  private async selectMode(mode: 'ios' | 'md') {
    this.demoMode = mode;
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="primary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.editor.demo}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-item>
            <ion-input
              value={this.demoSrc}
              placeholder={i18n.state.editor.enter_demo_url}
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
            ></ion-input>
          </ion-item>
        </ion-list>

        {this.renderDevices()}

        <ion-button
          disabled={this.demoSrc === undefined || !this.demoSrc || this.demoSrc === ''}
          color="dark"
          shape="round"
          onClick={() => this.save()}
          class="ion-margin-top"
        >
          <ion-label>{i18n.state.core.save}</ion-label>
        </ion-button>
      </ion-content>
    ];
  }

  private renderDevices() {
    return (
      <ion-radio-group value={this.demoMode} class="devices">
        {this.renderDevice('md')}
        {this.renderDevice('ios')}
      </ion-radio-group>
    );
  }

  private renderDevice(mode: 'md' | 'ios') {
    return (
      <article custom-tappable onClick={() => this.selectMode(mode)} class="ion-padding">
        <deckgo-demo mode={mode}></deckgo-demo>

        <div class="ion-margin-top">
          <ion-radio value={mode} mode="md"></ion-radio>
          <ion-label>{mode === 'md' ? 'Android' : 'iOS'}</ion-label>
        </div>
      </article>
    );
  }
}
