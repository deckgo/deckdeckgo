import {Component, Element, h, Prop, Host, JSX} from '@stencil/core';

import userStore from '../../../../../stores/user.store';

import {SocialUtils} from '../../../../../utils/editor/social.utils';
import {HeaderFooterUtils} from '../../../../../utils/editor/header-footer.utils';

@Component({
  tag: 'app-deck-header-footer',
  styleUrl: 'app-deck-header-footer.scss',
})
export class AppDeckHeaderFooter {
  @Element() el: HTMLElement;

  @Prop()
  deckElement: HTMLElement;

  private async appendHeaderFooter(slotName: 'header' | 'footer', type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom') {
    console.log(this.deckElement);

    await HeaderFooterUtils.append(this.deckElement, userStore.state.user, slotName, type);
  }

  render() {
    return (
      <Host>
        {this.renderHeaderFooter('header')}

        {this.renderHeaderFooter('footer')}
      </Host>
    );
  }

  private renderHeaderFooter(type: 'header' | 'footer') {
    if (!userStore.state.user || !userStore.state.user.data || !userStore.state.user.data.social) {
      return undefined;
    }

    return (
      <app-expansion-panel>
        <ion-label slot="title">{type === 'header' ? 'Header' : 'Footer'}</ion-label>
        <div class="container ion-margin-bottom">
          {this.renderTwitter(type)}
          {this.renderLinkedin(type)}
          {this.renderDev(type)}
          {this.renderMedium(type)}
          {this.renderGitHub(type)}
          {this.renderCustom(type)}
        </div>
      </app-expansion-panel>
    );
  }

  private renderTwitter(slotName: 'header' | 'footer') {
    if (
      !userStore.state.user.data.social.twitter ||
      userStore.state.user.data.social.twitter === '' ||
      userStore.state.user.data.social.twitter === undefined
    ) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createTwitter(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'twitter')}>{link}</button>;
  }

  private renderLinkedin(slotName: 'header' | 'footer') {
    if (
      !userStore.state.user.data.social.linkedin ||
      userStore.state.user.data.social.linkedin === '' ||
      userStore.state.user.data.social.linkedin === undefined
    ) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createLinkedin(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'linkedin')}>{link}</button>;
  }

  private renderDev(slotName: 'header' | 'footer') {
    if (!userStore.state.user.data.social.dev || userStore.state.user.data.social.dev === '' || userStore.state.user.data.social.dev === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createDev(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'dev')}>{link}</button>;
  }

  private renderMedium(slotName: 'header' | 'footer') {
    if (!userStore.state.user.data.social.medium || userStore.state.user.data.social.medium === '' || userStore.state.user.data.social.medium === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createMedium(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'medium')}>{link}</button>;
  }

  private renderGitHub(slotName: 'header' | 'footer') {
    if (!userStore.state.user.data.social.github || userStore.state.user.data.social.github === '' || userStore.state.user.data.social.github === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createGitHub(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'github')}>{link}</button>;
  }

  private renderCustom(slotName: 'header' | 'footer') {
    if (!userStore.state.user.data.social.custom || userStore.state.user.data.social.custom === '' || userStore.state.user.data.social.custom === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createCustom(userStore.state.user);

    return <button onClick={() => this.appendHeaderFooter(slotName, 'custom')}>{link}</button>;
  }
}
