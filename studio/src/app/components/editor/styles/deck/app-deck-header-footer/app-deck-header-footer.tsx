import {Component, Element, h, Prop, Host, JSX, EventEmitter, State, Event} from '@stencil/core';

import userStore from '../../../../../stores/user.store';
import authStore from '../../../../../stores/auth.store';
import i18n from '../../../../../stores/i18n.store';

import {SocialUtils} from '../../../../../utils/editor/social.utils';
import {HeaderFooterUtils} from '../../../../../utils/editor/header-footer.utils';
import navStore, {NavDirection} from '../../../../../stores/nav.store';

import {renderI18n} from '../../../../../utils/core/i18n.utils';

@Component({
  tag: 'app-deck-header-footer',
  styleUrl: 'app-deck-header-footer.scss',
})
export class AppDeckHeaderFooter {
  @Element() el: HTMLElement;

  @Prop()
  deckElement: HTMLElement;

  @Prop()
  deckDidChange: EventEmitter<HTMLElement>;

  @Event()
  private navigateSettings: EventEmitter<void>;

  @Event()
  private navigateSignIn: EventEmitter<void>;

  @State()
  private headerType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined = undefined;

  @State()
  private footerType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined = undefined;

  async componentWillLoad() {
    this.headerType = await HeaderFooterUtils.currentType(this.deckElement, 'header');
    this.footerType = await HeaderFooterUtils.currentType(this.deckElement, 'footer');
  }

  private async appendHeaderFooter(slotName: 'header' | 'footer', type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom') {
    await HeaderFooterUtils.append(this.deckElement, userStore.state.user, slotName, type);

    this.deckDidChange.emit(this.deckElement);

    await this.updateType(slotName, type);
  }

  private async reset(slotName: 'header' | 'footer') {
    await HeaderFooterUtils.remove(this.deckElement, slotName);

    this.deckDidChange.emit(this.deckElement);

    await this.updateType(slotName, undefined);
  }

  private async updateType(slotName: 'header' | 'footer', type: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (slotName === 'header') {
      this.headerType = type;
    } else if (slotName === 'footer') {
      this.footerType = type;
    }
  }

  private async onNavigateSettings() {
    this.navigateSettings.emit();

    navStore.state.nav = {
      url: '/profile',
      direction: NavDirection.FORWARD,
    };
  }

  private async onNavigateSignIn() {
    this.navigateSignIn.emit();
  }

  private hasOneOptionAtLeast(): boolean {
    return (
      userStore.state.user?.data?.social &&
      (userStore.state.user.data.social.twitter !== null ||
        userStore.state.user.data.social.linkedin !== null ||
        userStore.state.user.data.social.dev !== null ||
        userStore.state.user.data.social.medium !== null ||
        userStore.state.user.data.social.github !== null ||
        userStore.state.user.data.social.custom !== null)
    );
  }

  render() {
    if (!authStore.state.loggedIn) {
      return this.renderNotLoggedIn();
    }

    return this.renderOptions();
  }

  private renderNotLoggedIn() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">{i18n.state.editor.header_footer}</ion-label>

        <div class="container ion-margin-bottom">
          <ion-label class="no-options">
            {renderI18n(i18n.state.editor.header_footer_sign_in, {
              placeholder: '{0}',
              value: <a onClick={() => this.onNavigateSignIn()}>{i18n.state.nav.sign_in.toLowerCase()}</a>,
            })}
          </ion-label>
        </div>
      </app-expansion-panel>
    );
  }

  private renderOptions() {
    return (
      <Host>
        {this.renderHeaderFooter('header', this.headerType)}

        {this.renderHeaderFooter('footer', this.footerType)}

        {this.optionsNotice()}
      </Host>
    );
  }

  private renderHeaderFooter(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (!userStore.state.user || !userStore.state.user.data || !userStore.state.user.data.social) {
      return undefined;
    }

    const options: boolean = this.hasOneOptionAtLeast();

    return (
      <app-expansion-panel>
        <ion-label slot="title">{slotName === 'header' ? 'Header' : 'Footer'}</ion-label>
        <div class="container ion-margin-bottom">
          {this.renderTwitter(slotName, selectedType)}
          {this.renderLinkedin(slotName, selectedType)}
          {this.renderDev(slotName, selectedType)}
          {this.renderMedium(slotName, selectedType)}
          {this.renderGitHub(slotName, selectedType)}
          {this.renderCustom(slotName, selectedType)}
          {this.renderNoOptions(options)}
        </div>

        {this.renderReset(slotName, selectedType, options)}
      </app-expansion-panel>
    );
  }

  private renderNoOptions(options: boolean) {
    if (options) {
      return undefined;
    }

    return <ion-label class="no-options">{i18n.state.editor.no_options}</ion-label>;
  }

  private renderReset(
    slotName: 'header' | 'footer',
    selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined,
    options: boolean
  ) {
    if (!options) {
      return undefined;
    }

    return (
      <ion-item class="action-button">
        <ion-button shape="round" onClick={() => this.reset(slotName)} fill="outline" class="delete" disabled={selectedType === undefined}>
          <ion-label>{i18n.state.core.reset}</ion-label>
        </ion-button>
      </ion-item>
    );
  }

  private renderTwitter(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (
      !userStore.state.user.data.social.twitter ||
      userStore.state.user.data.social.twitter === '' ||
      userStore.state.user.data.social.twitter === undefined
    ) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createTwitter(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'twitter')} class={selectedType === 'twitter' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private renderLinkedin(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (
      !userStore.state.user.data.social.linkedin ||
      userStore.state.user.data.social.linkedin === '' ||
      userStore.state.user.data.social.linkedin === undefined
    ) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createLinkedin(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'linkedin')} class={selectedType === 'linkedin' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private renderDev(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (!userStore.state.user.data.social.dev || userStore.state.user.data.social.dev === '' || userStore.state.user.data.social.dev === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createDev(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'dev')} class={selectedType === 'dev' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private renderMedium(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (!userStore.state.user.data.social.medium || userStore.state.user.data.social.medium === '' || userStore.state.user.data.social.medium === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createMedium(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'medium')} class={selectedType === 'medium' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private renderGitHub(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (!userStore.state.user.data.social.github || userStore.state.user.data.social.github === '' || userStore.state.user.data.social.github === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createGitHub(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'github')} class={selectedType === 'github' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private renderCustom(slotName: 'header' | 'footer', selectedType: 'twitter' | 'linkedin' | 'dev' | 'medium' | 'github' | 'custom' | undefined) {
    if (!userStore.state.user.data.social.custom || userStore.state.user.data.social.custom === '' || userStore.state.user.data.social.custom === undefined) {
      return undefined;
    }

    const link: JSX.IntrinsicElements = SocialUtils.createCustom(userStore.state.user);

    return (
      <button onClick={() => this.appendHeaderFooter(slotName, 'custom')} class={selectedType === 'custom' ? 'selected' : undefined}>
        {link}
      </button>
    );
  }

  private optionsNotice() {
    return (
      <div class="ion-padding-start ion-padding-end ion-padding-top">
        {renderI18n(i18n.state.editor.header_footer_edit, {
          placeholder: '{0}',
          value: <a onClick={() => this.onNavigateSettings()}>{i18n.state.nav.settings.toLowerCase()}</a>,
        })}
      </div>
    );
  }
}
