import {Component, Element, Fragment, h} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';

import {cloud} from '../../../utils/core/environment.utils';

import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-menu',
  styleUrl: 'app-menu.scss',
  shadow: false
})
export class AppMenu {
  @Element() el: HTMLElement;

  private signIn: boolean = cloud();

  render() {
    return (
      <ion-list>
        {this.renderUser()}

        {this.renderEditor()}

        {this.renderDecks()}
        {this.renderDocs()}
        {this.renderStorage()}

        {this.renderSettings()}

        {this.renderInteract()}
      </ion-list>
    );
  }

  private renderUser() {
    if (authStore.state.loggedIn) {
      return (
        <ion-item class="user">
          <app-user-info displayAvatar={true}></app-user-info>
        </ion-item>
      );
    } else {
      return <ion-item class="user"></ion-item>;
    }
  }

  private renderEditor() {
    return (
      <ion-item button href="/" routerDirection="forward">
        <AppIcon name="pencil" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.menu.editor}</ion-label>
      </ion-item>
    );
  }

  private renderDecks() {
    if (!this.signIn) {
      return undefined;
    }

    return (
      <ion-item button href="/decks" routerDirection="forward">
        <AppIcon name="deck" path="icons" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.menu.presentations}</ion-label>
      </ion-item>
    );
  }

  private renderDocs() {
    if (!this.signIn) {
      return undefined;
    }

    return (
      <ion-item button href="/docs" routerDirection="forward">
        <AppIcon name="doc" path="icons" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.menu.documents}</ion-label>
      </ion-item>
    );
  }

  private renderStorage() {
    if (!this.signIn) {
      return undefined;
    }

    return (
      <ion-item button href="/storage" routerDirection="forward">
        <AppIcon name="storage" path="icons" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
        <ion-label>{i18n.state.menu.assets}</ion-label>
      </ion-item>
    );
  }

  private renderInteract() {
    return (
      <Fragment>
        <ion-item button href="/poll" routerDirection="forward">
          <AppIcon name="chatbubble-ellipses" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
          <ion-label>{i18n.state.menu.poll}</ion-label>
        </ion-item>

        <ion-item button class="home remote" href="https://deckdeckgo.app" target="_blank">
          <AppIcon name="phone-portrait" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
          <ion-label>{i18n.state.menu.remote_control}</ion-label>
        </ion-item>
      </Fragment>
    );
  }

  private renderSettings() {
    if (!this.signIn) {
      return this.renderCustomization();
    }

    return (
      <app-expansion-panel expanded="close">
        <ion-label slot="title">{i18n.state.nav.settings}</ion-label>
        <AppIcon name="settings" ariaLabel="" ariaHidden={true} lazy={true} slot="icon"></AppIcon>

        <ion-list class="settings">
          <ion-item button href="/profile" routerDirection="forward">
            <ion-label>{i18n.state.nav.profile}</ion-label>
            <AppIcon name="person" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
          </ion-item>
          {this.renderCustomization()}
          <ion-item button href="/templates" routerDirection="forward">
            <ion-label>{i18n.state.nav.templates}</ion-label>
            <AppIcon name="reader" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderCustomization() {
    return (
      <ion-item button href="/customization" routerDirection="forward">
        <ion-label>{i18n.state.nav.customization}</ion-label>
        <AppIcon name="color-palette" ariaLabel="" ariaHidden={true} lazy={true} slot="start"></AppIcon>
      </ion-item>
    );
  }
}
