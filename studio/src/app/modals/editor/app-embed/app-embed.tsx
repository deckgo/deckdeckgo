import {Component, Element, h, Listen, State} from '@stencil/core';

import deckStore from '../../../stores/deck.store';

import {getPublishedUrl} from '../../../utils/core/share.utils';

@Component({
  tag: 'app-embed',
  styleUrl: 'app-embed.scss',
})
export class AppEmbed {
  @Element() el: HTMLElement;

  @State()
  private embedCode: string = undefined;

  private embedCodeElement!: HTMLIonTextareaElement;

  async componentWillLoad() {
    const url: string = await getPublishedUrl(deckStore.state.deck);
    this.embedCode = `<iframe src="${url}?embed" width="576" height="420" scrolling="no" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>`;
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(false);
  }

  private async selectEmbedCode() {
    if (!this.embedCodeElement) {
      return;
    }

    await this.embedCodeElement.setFocus();

    const textarea: HTMLTextAreaElement = await this.embedCodeElement.getInputElement();
    if (textarea) {
      textarea.select();
    }
  }

  private async copyEmbedCode() {
    if (!document) {
      return;
    }

    await this.selectEmbedCode();

    document.execCommand('copy');
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
          <ion-title class="ion-text-uppercase">Embed</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <ion-list class="inputs-list">
          <ion-item class="item-title">
            <ion-label>Copy the following code to embed your presentation.</ion-label>
          </ion-item>
          <ion-item>
            <ion-textarea
              rows={4}
              value={this.embedCode}
              readonly={true}
              ref={(el) => (this.embedCodeElement = el as HTMLIonTextareaElement)}
              onClick={() => this.selectEmbedCode()}></ion-textarea>
          </ion-item>
        </ion-list>

        <ion-button color="primary" shape="round" onClick={() => this.copyEmbedCode()}>
          <ion-label>Copy to clipboard</ion-label>
        </ion-button>
      </ion-content>,
    ];
  }
}
