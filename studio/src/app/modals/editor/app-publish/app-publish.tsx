import {Component, Element, Listen, h, State} from '@stencil/core';

import {Deck} from '@deckdeckgo/editor';

import i18n from '../../../stores/i18n.store';
import editorStore from '../../../stores/editor.store';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

import {snapshotDeck} from '../../../providers/data/deck/deck.provider';

import {updateSlidesQRCode} from '../../../utils/editor/qrcode.utils';

@Component({
  tag: 'app-publish',
  styleUrl: 'app-publish.scss'
})
export class AppPublish {
  @Element() el: HTMLElement;

  @State()
  private publishedUrl: string;

  private unsubscribeSnapshot: () => void | undefined;

  async componentWillLoad() {
    this.unsubscribeSnapshot = await snapshotDeck({
      deckId: editorStore.state.deck.id,
      onNext: (snapshot: Deck) => (editorStore.state.deck = {...snapshot})
    });
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    this.unsubscribeSnapshot?.();

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private published({detail}: CustomEvent<string>) {
    this.publishedUrl = detail;

    updateSlidesQRCode(this.publishedUrl);
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="tertiary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          {this.renderTitle()}
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding fullscreen-padding">
        <main
          class={
            this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '' ? 'published ion-padding' : 'ion-padding'
          }>
          {this.renderMain()}
        </main>
      </ion-content>
    ];
  }

  private renderTitle() {
    if (this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '') {
      return <ion-title class="ion-text-uppercase">{i18n.state.editor.published}</ion-title>;
    } else {
      return <ion-title class="ion-text-uppercase">{i18n.state.nav.ready_to_share}</ion-title>;
    }
  }

  private renderMain() {
    if (this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '') {
      return <app-publish-done publishedUrl={this.publishedUrl}></app-publish-done>;
    } else {
      return <app-publish-edit onPublished={($event: CustomEvent<string>) => this.published($event)}></app-publish-edit>;
    }
  }
}
