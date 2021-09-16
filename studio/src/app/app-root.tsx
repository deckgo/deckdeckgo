import {Component, Element, h, Listen, State} from '@stencil/core';

import errorStore from './stores/error.store';
import navStore from './stores/nav.store';
import shareStore, {ShareData} from './stores/share.store';
import authStore from './stores/auth.store';

import {initAuthProvider} from './providers/auth/auth.provider';

import {ThemeService} from './services/theme/theme.service';
import {NavDirection, NavParams} from './stores/nav.store';
import {ColorService} from './services/editor/color/color.service';
import {SettingsService} from './services/settings/settings.service';
import {LangService} from './services/lang/lang.service';
import {initSyncState} from './providers/sync/sync.provider';

import {toastController} from './utils/ionic/ionic.overlay';

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.scss'
})
export class AppRoot {
  @Element() el: HTMLElement;

  private readonly themeService: ThemeService;
  private readonly colorService: ColorService;
  private readonly settingsService: SettingsService;
  private readonly langService: LangService;

  @State()
  private loading: boolean = true;

  private destroyErrorListener;
  private destroyNavListener;
  private destroyShareListener;
  private destroyAuthListener;

  private shareRef!: HTMLAppShareDeckElement;

  constructor() {
    this.themeService = ThemeService.getInstance();
    this.colorService = ColorService.getInstance();
    this.settingsService = SettingsService.getInstance();
    this.langService = LangService.getInstance();
  }

  async componentWillLoad() {
    this.destroyAuthListener = authStore.onChange('authUser', async () => await initSyncState());

    const promises: Promise<void>[] = [
      initAuthProvider(),
      this.themeService.initDarkModePreference(),
      this.colorService.init(),
      this.settingsService.init(),
      this.langService.init()
    ];

    Promise.all(promises).then(() => {
      // async componentWillLoad is render blocking and we want to display the app as soon as possible
    });
  }

  async componentDidLoad() {
    this.loading = false;

    this.destroyErrorListener = errorStore.onChange('error', (error: string | undefined) => {
      if (error && error !== undefined) {
        this.toastError(error);
      }
    });

    this.destroyNavListener = navStore.onChange('nav', async (params: NavParams | undefined) => {
      await this.navigate(params);
    });

    this.destroyShareListener = shareStore.onChange('share', async (share: ShareData | null) => {
      await this.openShare(share);
    });
  }

  disconnectedCallback() {
    this.destroyErrorListener?.();
    this.destroyNavListener?.();
    this.destroyShareListener?.();
    this.destroyAuthListener?.();
  }

  @Listen('swUpdate', {target: 'window'})
  async onSWUpdate() {
    const registration = await navigator.serviceWorker.getRegistration();

    if (!registration || !registration.waiting) {
      return;
    }

    const toast: HTMLIonToastElement = await toastController.create({
      message: 'A new version is available, reload to update.',
      buttons: [
        {
          text: 'Reload',
          icon: 'refresh-circle-outline',
          handler: () => {
            registration.waiting.postMessage('skipWaiting');
            window.location.reload();
          }
        }
      ],
      position: 'top',
      color: 'quaternary'
    });

    await toast.present();
  }

  private async toastError(error: string) {
    const toast: HTMLIonToastElement = await toastController.create({
      message: error,
      buttons: [
        {
          text: 'Close',
          role: 'cancel'
        }
      ],
      position: 'top',
      color: 'danger',
      duration: 6000
    });

    toast.onDidDismiss().then(() => {
      errorStore.state.error = undefined;
    });

    await toast.present();
  }

  private async navigate(params: NavParams) {
    if (!params) {
      return;
    }

    const router: HTMLIonRouterElement = this.el.querySelector('ion-router');

    if (!router) {
      return;
    }

    if (params.direction === NavDirection.RELOAD) {
      window.location.assign(params.url);
    } else if (params.direction === NavDirection.ROOT) {
      await router.push(params.url, 'root');
    } else if (params.direction === NavDirection.BACK) {
      await router.back();
    } else {
      await router.push(params.url);
    }

    navStore.reset();
  }

  private async openShare(share: ShareData | null) {
    if (!share) {
      return;
    }

    if (!this.shareRef) {
      return;
    }

    await this.shareRef.openShare();
  }

  /**
   * Note: Routes need to be flat as we path the return and deckId (redirect) to the signin route. So no /settings/something but /something.
   */
  render() {
    return [
      <ion-app class={this.loading ? 'loading' : undefined}>
        <ion-router useHash={false}>
          <ion-route url="/" component="app-editor" />

          <ion-route url="/profile" component="app-profile" />
          <ion-route url="/customization" component="app-customization" />
          <ion-route url="/templates" component="app-templates" />

          <ion-route url="/dashboard" component="app-dashboard-page" />

          <ion-route url="/signin" component="app-signin-page" />
          <ion-route url="/signin/:redirect" component="app-signin-page" />

          <ion-route url="/poll" component="app-poll" />
          <ion-route url="/poll/:pollKey" component="app-poll" />
        </ion-router>

        <ion-menu id="ion-menu" side="start" type="overlay" swipeGesture={false} content-id="menu-content">
          <ion-content>
            <ion-menu-toggle autoHide={false}>
              <app-menu></app-menu>

              <app-links></app-links>
            </ion-menu-toggle>
          </ion-content>
        </ion-menu>

        <ion-nav id="menu-content" />

        <app-share-deck ref={(el) => (this.shareRef = el as HTMLAppShareDeckElement)}></app-share-deck>
      </ion-app>
    ];
  }
}
