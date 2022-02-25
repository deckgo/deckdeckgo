import {StorageFile, throwError, User} from '@deckdeckgo/editor';
import {clearEdit} from '@deckdeckgo/offline';
import {deleteAuth, updateUser} from '@deckdeckgo/sync';
import type {OverlayEventDetail} from '@ionic/core';
import {loadingController, modalController} from '@ionic/core';
import {Component, Element, Fragment, h, Listen, State} from '@stencil/core';
import {EnvironmentDeckDeckGoConfig} from '../../../../config/environment-config';
import {uploadOnlineFile} from '../../../../providers/storage/storage.provider';
import {ImageHistoryService} from '../../../../services/editor/image-history/image-history.service';
import {EnvironmentConfigService} from '../../../../services/environment/environment-config.service';
import authStore from '../../../../stores/auth.store';
import i18n from '../../../../stores/i18n.store';
import navStore, {NavDirection} from '../../../../stores/nav.store';
import userStore from '../../../../stores/user.store';
import {firebase} from '../../../../utils/core/environment.utils';
import {renderI18n} from '../../../../utils/core/i18n.utils';
import {signIn} from '../../../../utils/core/signin.utils';
import {UserUtils} from '../../../../utils/core/user.utils';

@Component({
  tag: 'app-profile',
  styleUrl: 'app-profile.scss'
})
export class AppProfile {
  @Element() el: HTMLElement;

  @State()
  private user: User;

  @State()
  private valid: boolean = true;

  private validUsername: boolean = true;
  private validName: boolean = true;
  private validEmail: boolean = true;

  @State()
  private saving: boolean = false;

  private imageHistoryService: ImageHistoryService;

  private profilePicture: File;

  private customLogo: File;

  @State()
  private twitter: string = undefined;

  @State()
  private linkedin: string = undefined;

  @State()
  private dev: string = undefined;

  @State()
  private medium: string = undefined;

  @State()
  private github: string = undefined;

  @State()
  private custom: string = undefined;

  private destroyUserListener;
  private destroyApiUserListener;

  private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  private firebase: boolean = firebase();

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

  async componentDidLoad() {
    this.destroyListener();

    await this.initUser();
  }

  private async initUser() {
    if (userStore.state.user && authStore.state.loggedIn) {
      await this.initUserData();
      return;
    }

    this.destroyUserListener = userStore.onChange('user', async () => {
      await this.initUserData();
    });
  }

  disconnectedCallback() {
    this.destroyListener();
  }

  private destroyListener() {
    if (this.destroyUserListener) {
      this.destroyUserListener();
    }

    if (this.destroyApiUserListener) {
      this.destroyApiUserListener();
    }
  }

  private async initUserData() {
    if (!userStore.state.user || userStore.state.user === undefined || !userStore.state.user.data) {
      return;
    }

    this.user = {...userStore.state.user};

    this.validateNameInput();
    this.validateEmailInput();

    if (!userStore.state.user.data.social) {
      return;
    }

    this.twitter = this.user.data.social.twitter;
    this.linkedin = this.user.data.social.linkedin;
    this.dev = this.user.data.social.dev;
    this.medium = this.user.data.social.medium;
    this.github = this.user.data.social.github;
    this.custom = this.user.data.social.custom;
  }

  @Listen('keydown')
  async handleEnterKey($event: KeyboardEvent) {
    if ($event && $event.key === 'Enter') {
      await this.save();
    }
  }

  private async handleSubmit(e: Event) {
    e.preventDefault();

    await this.save();
  }

  private handleUsernameInput($event: CustomEvent<KeyboardEvent>) {
    this.user.data.username = ($event.target as InputTargetEvent).value;
  }

  private validateUsernameInput() {
    if (!this.firebase) {
      this.validUsername =
        this.user.data.username === null ||
        this.user.data.username === undefined ||
        this.user.data.username === '' ||
        UserUtils.validUsername(this.user.data.username);
      this.isValid();

      return;
    }

    this.validUsername = UserUtils.validUsername(this.user.data.username);
    this.isValid();
  }

  private handleNameInput($event: CustomEvent<KeyboardEvent>) {
    this.user.data.name = ($event.target as InputTargetEvent).value;
  }

  private validateNameInput() {
    if (!this.firebase) {
      this.validName =
        this.user &&
        this.user.data &&
        (this.user.data.name === null ||
          this.user.data.name === undefined ||
          this.user.data.name === '' ||
          UserUtils.validName(this.user.data.name));
      this.isValid();

      return;
    }

    this.validName = this.user && this.user.data && UserUtils.validName(this.user.data.name);
    this.isValid();
  }

  private handleEmailInput($event: CustomEvent<KeyboardEvent>) {
    this.user.data.email = ($event.target as InputTargetEvent).value;
  }

  private validateEmailInput() {
    if (!this.firebase) {
      this.validEmail =
        this.user &&
        this.user.data &&
        (this.user.data.email === null ||
          this.user.data.email === undefined ||
          this.user.data.email === '' ||
          UserUtils.validEmail(this.user.data.email));
      this.isValid();

      return;
    }

    this.validEmail = this.user && this.user.data && UserUtils.validEmail(this.user.data.email);
    this.isValid();
  }

  private isValid() {
    this.valid = this.validUsername && this.validName && this.validEmail;
  }

  private toggleNewsletter($event: CustomEvent) {
    if (this.user && this.user.data) {
      this.user.data.newsletter = $event.detail.checked;
    }
  }

  private handleSocialInput($event: CustomEvent<KeyboardEvent>, key: string) {
    if (this.user && this.user.data) {
      if (!this.user.data.social) {
        this.user.data.social = {};
      }

      const value: string = ($event.target as InputTargetEvent).value;

      if (!value || value === '') {
        this.user.data.social[key] = null;
        this[key] = undefined;
      } else {
        this.user.data.social[key] = value;
        this[key] = value;
      }
    }
  }

  private handleSummaryInput($event: CustomEvent<KeyboardEvent>) {
    this.user.data.bio = ($event.target as InputTargetEvent).value;
  }

  private save(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.valid) {
        resolve();
        return;
      }

      const loading: HTMLIonLoadingElement = await loadingController.create({});

      await loading.present();

      try {
        this.saving = true;

        await this.uploadProfilePicture();
        await this.uploadCustomLogo();

        await this.saveUser();

        this.saving = false;
      } catch (err) {
        throwError(err);
        this.saving = false;
      }

      await loading.dismiss();

      resolve();
    });
  }

  private saveUser(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!this.valid) {
        resolve();
        return;
      }

      try {
        await updateUser(this.user);

        resolve();
      } catch (err) {
        reject("Your changes couldn't be saved");
      }
    });
  }

  private uploadProfilePicture(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!this.valid || !this.user || !this.user.data) {
        resolve();
        return;
      }

      if (!this.profilePicture) {
        resolve();
        return;
      }

      try {
        const storageFile: StorageFile = await uploadOnlineFile(this.profilePicture, 'avatars', 524288);

        if (storageFile) {
          this.user.data.photo_url = storageFile.downloadUrl;
        }

        resolve();
      } catch (err) {
        reject(`Could not upload your profile picture! ${err}`);
      }
    });
  }

  private uploadCustomLogo(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!this.valid || !this.user || !this.user.data) {
        resolve();
        return;
      }

      if (!this.user.data.social || !this.user.data.social.custom) {
        resolve();
        return;
      }

      if (!this.customLogo) {
        resolve();
        return;
      }

      try {
        const storageFile: StorageFile = await uploadOnlineFile(this.customLogo, 'images', 524288);

        if (storageFile) {
          this.user.data.social.custom_logo_url = storageFile.downloadUrl;
        }

        resolve();
      } catch (err) {
        reject('Could not upload your profile picture!');
      }
    });
  }

  private async presentConfirmDelete() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-user-delete',
      componentProps: {
        username: this.user.data.username || 'deckdeckgo'
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        await this.deleteUser();
      }
    });

    await modal.present();
  }

  private deleteUser(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        const loading: HTMLIonLoadingElement = await loadingController.create({});

        await loading.present();

        await deleteAuth();

        await this.imageHistoryService.clear();

        await clearEdit(true);

        navStore.state.nav = {
          url: '/',
          direction: NavDirection.RELOAD
        };

        await loading.dismiss();

        resolve();
      } catch (err) {
        throwError(
          "Your user couldn't be deleted. Sign out and in again prior trying out again. If it still does not work, contact us per email."
        );
      }
    });
  }

  private async selectProfilePicture() {
    const filePicker: HTMLInputElement = this.el.querySelector('input#inputProfilePicture');

    if (!filePicker) {
      return;
    }

    if (filePicker.files && filePicker.files.length > 0) {
      this.profilePicture = filePicker.files[0];
    }
  }

  private async selectCustomLogo() {
    const filePicker: HTMLInputElement = this.el.querySelector('input#inputCustomLogo');

    if (!filePicker) {
      return;
    }

    if (filePicker.files && filePicker.files.length > 0) {
      this.customLogo = filePicker.files[0];
    }
  }

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content class="ion-padding fullscreen-padding">
        <main class="ion-padding fit">
          <h1>{i18n.state.settings.profile}</h1>
          {this.renderGuardedContent()}
        </main>
      </ion-content>
    ];
  }

  private renderGuardedContent() {
    if (!authStore.state.authUser) {
      return this.renderNotLoggedInContent();
    }

    return (
      <Fragment>
        {this.renderUserContent()}
        {this.renderDangerZone()}
      </Fragment>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_settings, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  }

  private renderUserContent() {
    return [
      <form onSubmit={(e: Event) => this.handleSubmit(e)}>
        <ion-list class="inputs-list">
          {this.renderName()}
          {this.renderUsername()}
          {this.renderEmail()}
        </ion-list>

        {this.renderUserAvatar()}

        {this.renderSummary()}

        {this.renderSocial()}

        {this.renderSubmitForm()}
      </form>,
      <p class="info">{i18n.state.settings.profile_note}</p>
    ];
  }

  private renderName() {
    return [
      <ion-item class="item-title">
        <ion-label>{i18n.state.settings.name}</ion-label>
      </ion-item>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data ? this.user.data.name : undefined}
          debounce={500}
          minlength={3}
          maxlength={64}
          required={this.firebase}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleNameInput($event)}
          onIonChange={() => this.validateNameInput()}></ion-input>
      </ion-item>
    ];
  }

  private renderEmail() {
    if (!this.firebase) {
      return undefined;
    }

    return [
      <ion-item class="item-title">
        <ion-label>{i18n.state.settings.email}</ion-label>
      </ion-item>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data ? this.user.data.email : undefined}
          debounce={500}
          minlength={3}
          maxlength={254}
          required={this.firebase}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleEmailInput($event)}
          onIonChange={() => this.validateEmailInput()}></ion-input>
      </ion-item>,
      <div class="newsletter">
        <ion-label>{i18n.state.settings.newsletter}</ion-label>
        <ion-checkbox
          slot="end"
          value="pepperoni"
          checked={this.user && this.user.data ? this.user.data.newsletter : false}
          disabled={this.saving}
          onIonChange={($event: CustomEvent) => this.toggleNewsletter($event)}></ion-checkbox>
      </div>
    ];
  }

  private renderUsername() {
    if (!firebase()) {
      return undefined;
    }

    return [
      <ion-item class="item-title">
        <ion-label>{i18n.state.settings.username}</ion-label>
      </ion-item>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data ? this.user.data.username : undefined}
          debounce={500}
          minlength={3}
          maxlength={32}
          required={this.firebase}
          disabled={this.saving || !authStore.state.loggedIn}
          input-mode="text"
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleUsernameInput($event)}
          onIonChange={() => this.validateUsernameInput()}></ion-input>
      </ion-item>
    ];
  }

  private renderSubmitForm() {
    return (
      <ion-button
        type="submit"
        class="ion-margin-top"
        disabled={!this.valid || this.saving || !this.user || !authStore.state.loggedIn}
        color="primary"
        shape="round">
        <ion-label>{i18n.state.core.submit}</ion-label>
      </ion-button>
    );
  }

  private renderDangerZone() {
    return [
      <h1 class="danger-zone">{i18n.state.settings.danger_zone}</h1>,
      <p>{i18n.state.settings.no_way_back}</p>,
      <ion-button
        color="danger"
        shape="round"
        fill="outline"
        onClick={() => this.presentConfirmDelete()}
        disabled={this.saving || !authStore.state.authUser || !authStore.state.loggedIn}>
        <ion-label>{i18n.state.settings.delete_user}</ion-label>
      </ion-button>
    ];
  }

  private renderUserAvatar() {
    return (
      <ion-list class="inputs-list">
        <ion-item class="item-title">
          <ion-label>{i18n.state.settings.profile_picture}</ion-label>
        </ion-item>
        <div class="avatar">
          <app-avatar src={this.user && this.user.data ? this.user.data.photo_url : undefined} aria-label="Profile picture"></app-avatar>
          <input
            id="inputProfilePicture"
            type="file"
            accept="image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp"
            onChange={() => this.selectProfilePicture()}
            disabled={this.saving || !authStore.state.loggedIn}
          />
        </div>
        <p>
          <small>{i18n.state.settings.profile_picture_tips}</small>
        </p>
      </ion-list>
    );
  }

  private renderSummary() {
    return (
      <ion-list class="inputs-list">
        <ion-item class="item-title">
          <ion-label>{i18n.state.settings.summary}</ion-label>
        </ion-item>
        <ion-item>
          <ion-textarea
            rows={3}
            value={this.user && this.user.data ? this.user.data.bio : undefined}
            debounce={500}
            disabled={this.saving || !authStore.state.loggedIn}
            maxlength={192}
            placeholder={i18n.state.settings.bio}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleSummaryInput(e)}></ion-textarea>
        </ion-item>
      </ion-list>
    );
  }

  private renderSocial() {
    return (
      <ion-list class="inputs-list">
        {this.renderTwitter()}
        {this.renderLinkedIn()}
        {this.renderDev()}
        {this.renderMedium()}
        {this.renderGithub()}
        {this.renderCustom()}
      </ion-list>
    );
  }

  private renderTwitter() {
    return [
      <ion-item class="item-title">
        <ion-label>Twitter</ion-label>
      </ion-item>,
      <p>
        <small>
          https://twitter.com/<strong>{this.twitter ? this.twitter : 'yourusername'}</strong>
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.twitter : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'twitter')}></ion-input>
      </ion-item>
    ];
  }

  private renderLinkedIn() {
    return [
      <ion-item class="item-title">
        <ion-label>LinkedIn</ion-label>
      </ion-item>,
      <p>
        <small>
          https://www.linkedin.com/in/<strong>{this.linkedin ? this.linkedin : 'yourusername'}</strong>
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.linkedin : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'linkedin')}></ion-input>
      </ion-item>
    ];
  }

  private renderDev() {
    return [
      <ion-item class="item-title">
        <ion-label>Dev.to</ion-label>
      </ion-item>,
      <p>
        <small>
          https://dev.to/<strong>{this.dev ? this.dev : 'yourusername'}</strong>
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.dev : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'dev')}></ion-input>
      </ion-item>
    ];
  }

  private renderMedium() {
    return [
      <ion-item class="item-title">
        <ion-label>Medium</ion-label>
      </ion-item>,
      <p>
        <small>
          https://medium.com/@<strong>{this.medium ? this.medium : 'yourusername'}</strong>
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.medium : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'medium')}></ion-input>
      </ion-item>
    ];
  }

  private renderGithub() {
    return [
      <ion-item class="item-title">
        <ion-label>GitHub</ion-label>
      </ion-item>,
      <p>
        <small>
          https://github.com/<strong>{this.github ? this.github : 'yourusername'}</strong>
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.github : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'github')}></ion-input>
      </ion-item>
    ];
  }

  private renderCustom() {
    return [
      <ion-item class="item-title">
        <ion-label>{i18n.state.settings.custom}</ion-label>
      </ion-item>,
      <p>
        <small>
          {renderI18n(i18n.state.settings.custom_url, {
            placeholder: '{0}',
            value: <strong>{this.custom ? this.custom : 'https://yourwebsite.com'}</strong>
          })}
        </small>
      </p>,
      <ion-item>
        <ion-input
          value={this.user && this.user.data && this.user.data.social ? this.user.data.social.custom : undefined}
          debounce={500}
          maxlength={128}
          input-mode="text"
          disabled={this.saving || !authStore.state.loggedIn}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleSocialInput($event, 'custom')}></ion-input>
      </ion-item>,

      this.renderCustomLogo()
    ];
  }

  private renderCustomLogo() {
    return [
      <p class="ion-margin-top">
        <small>{i18n.state.settings.logo_address}</small>
      </p>,
      <div class="avatar">
        {this.user && this.user.data && this.user.data.social && this.user.data.social.custom_logo_url ? (
          <deckgo-lazy-img
            slot="icon"
            img-src={this.user.data.social.custom_logo_url}
            aria-label={i18n.state.settings.custom_logo}></deckgo-lazy-img>
        ) : (
          <deckgo-lazy-img
            slot="icon"
            svg-src={`${this.config.globalAssetsUrl}/icons/ionicons/globe.svg`}
            aria-label={i18n.state.settings.custom_logo}></deckgo-lazy-img>
        )}
        <input
          id="inputCustomLogo"
          type="file"
          accept="image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp"
          onChange={() => this.selectCustomLogo()}
          disabled={
            this.saving ||
            !this.user ||
            !this.user.data ||
            !this.user.data.social ||
            !this.user.data.social.custom ||
            !authStore.state.loggedIn
          }
        />
      </div>
    ];
  }
}
