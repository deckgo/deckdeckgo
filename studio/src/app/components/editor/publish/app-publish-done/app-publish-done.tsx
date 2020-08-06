import {Component, Element, h, Prop, State} from '@stencil/core';

import deckStore from '../../../../stores/deck.store';
import userStore from '../../../../stores/user.store';
import shareStore from '../../../../stores/share.store';

@Component({
  tag: 'app-publish-done',
  styleUrl: 'app-publish-done.scss',
})
export class AppPublishDone {
  @Element() el: HTMLElement;

  @Prop()
  publishedUrl: string;

  @State()
  private keywordIndex: number = Math.floor(Math.random() * 4);

  private keywords: string[] = ['Hooray', 'You did it', 'Applause', 'Thumbs up'];

  private share() {
    shareStore.state.share = {
      deck: deckStore.state.deck,
      userName: userStore.state.name,
      userSocial: userStore.state.social,
    };
  }

  render() {
    return (
      <article>
        <app-random-gif keyword={this.keywords[this.keywordIndex]}></app-random-gif>

        <h1 class="ion-text-center">{this.keywords[this.keywordIndex]}! Your presentation has been published.</h1>

        <p class="ion-text-center">
          It's time to <a onClick={() => this.share()}>share</a> it with the world, your colleagues, friends and community.
        </p>

        <ion-button color="tertiary" shape="round" onClick={() => this.share()} class="ion-margin">
          <ion-icon name="share-outline" slot="start"></ion-icon>
          <ion-label>Share</ion-label>
        </ion-button>

        <ion-label class="published-url ion-padding ion-text-center">
          Or{' '}
          <a href={this.publishedUrl} target="_blank" rel="noopener noreferrer">
            click here
          </a>{' '}
          to open it.
        </ion-label>
      </article>
    );
  }
}
