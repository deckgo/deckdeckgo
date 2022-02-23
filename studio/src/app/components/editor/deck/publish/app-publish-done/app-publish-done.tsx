import {Component, Element, h, Prop, State} from '@stencil/core';
import authStore from '../../../../../stores/auth.store';
import editorStore from '../../../../../stores/editor.store';
import i18n from '../../../../../stores/i18n.store';
import {renderI18n} from '../../../../../utils/core/i18n.utils';
import {share} from '../../../../../utils/core/share.utils';
import {AppIcon} from '../../../../core/app-icon/app-icon';

@Component({
  tag: 'app-publish-done',
  styleUrl: 'app-publish-done.scss'
})
export class AppPublishDone {
  @Element() el: HTMLElement;

  @Prop()
  publishedUrl: string;

  @State()
  private keywordIndex: number = Math.floor(Math.random() * 4);

  private keywords: string[] = [
    i18n.state.publish_done.hooray,
    i18n.state.publish_done.did_it,
    i18n.state.publish_done.applause,
    i18n.state.publish_done.thumbs_up
  ];

  render() {
    return (
      <article>
        <app-random-gif keyword={this.keywords[this.keywordIndex]}></app-random-gif>

        <h1 class="ion-text-center">
          {this.keywords[this.keywordIndex]}! {i18n.state.publish_done.published}
        </h1>

        <p class="ion-text-center">
          {renderI18n(i18n.state.publish_done.share, {
            placeholder: '{0}',
            value: <a onClick={() => share()}>{i18n.state.editor.share}</a>
          })}
        </p>

        <ion-button color="tertiary" shape="round" onClick={() => share()} class="ion-margin">
          <AppIcon name="share" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
          <ion-label>{i18n.state.editor.share}</ion-label>
        </ion-button>

        {this.renderGitHub()}
      </article>
    );
  }

  private renderGitHub() {
    if (!authStore.state.gitHub) {
      return undefined;
    }

    if (
      !editorStore.state.deck ||
      !editorStore.state.deck.data ||
      !editorStore.state.deck.data.github ||
      !editorStore.state.deck.data.github.publish
    ) {
      return undefined;
    }

    if (!editorStore.state.deck.data.github || !editorStore.state.deck.data.github.repo) {
      return (
        <ion-label class="published-url ion-padding ion-text-center">
          {renderI18n(i18n.state.publish_done.source_processing, {
            placeholder: '{0}',
            value: <ion-spinner color="tertiary"></ion-spinner>
          })}
        </ion-label>
      );
    }

    return (
      <ion-label class="published-url ion-padding ion-text-center">
        {renderI18n(i18n.state.publish_done.source_submitted, {
          placeholder: '{0}',
          value: (
            <a href={`${editorStore.state.deck.data.github.repo.url}/pulls`} target="_blank" rel="noopener noreferrer">
              {i18n.state.publish_done.repository}
            </a>
          )
        })}{' '}
        <AppIcon name="github" ariaLabel="" ariaHidden={true}></AppIcon>.
      </ion-label>
    );
  }
}
