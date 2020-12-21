import {Build, Component, Element, h, Listen, State} from '@stencil/core';

import {toastController} from '@ionic/core';

import errorStore from './stores/error.store';
import navStore from './stores/nav.store';
import shareStore, {ShareData} from './stores/share.store';

import {AuthService} from './services/auth/auth.service';

import {ThemeService} from './services/theme/theme.service';
import {OfflineService} from './services/editor/offline/offline.service';
import {NavDirection, NavParams} from './stores/nav.store';
import {ColorService} from './services/color/color.service';

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.scss',
})
export class AppRoot {
  @Element() el: HTMLElement;

  private authService: AuthService;

  private themeService: ThemeService;

  private colorService: ColorService;

  private offlineService: OfflineService;

  @State()
  private loading: boolean = true;

  private destroyErrorListener;
  private destroyNavListener;
  private destroyShareListener;

  private shareRef!: HTMLAppShareDeckElement;

  constructor() {
    this.authService = AuthService.getInstance();
    this.themeService = ThemeService.getInstance();
    this.colorService = ColorService.getInstance();
    this.offlineService = OfflineService.getInstance();
  }

  async componentWillLoad() {
    if (Build.isBrowser) {
      await this.authService.init();
      await this.themeService.initDarkModePreference();
      await this.colorService.init();
      await this.offlineService.init();
    }
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
    if (this.destroyErrorListener) {
      this.destroyErrorListener();
    }

    if (this.destroyNavListener) {
      this.destroyNavListener();
    }

    if (this.destroyShareListener) {
      this.destroyShareListener();
    }
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
          },
        },
      ],
      position: 'top',
      color: 'quaternary',
    });

    await toast.present();
  }

  private async toastError(error: string) {
    const toast: HTMLIonToastElement = await toastController.create({
      message: error,
      buttons: [
        {
          text: 'Close',
          role: 'cancel',
        },
      ],
      position: 'top',
      color: 'danger',
      duration: 6000,
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

  render() {
    return [
      <ion-app class={this.loading ? 'loading' : undefined}>
        <ion-router useHash={false}>
          <ion-route url="/" component="app-dashboard" />

          <ion-route url="/editor" component="app-editor" />
          <ion-route url="/editor/:deckId" component="app-editor" />

          <ion-route url="/settings" component="app-profile" />
          <ion-route url="/settings/profile" component="app-profile" />
          <ion-route url="/settings/customization" component="app-customization" />

          <ion-route url="/dashboard" component="app-dashboard" />

          <ion-route url="/signin" component="app-signin" />
          <ion-route url="/signin/:redirect" component="app-signin" />

          <ion-route url="/poll" component="app-poll" />
          <ion-route url="/poll/:pollKey" component="app-poll" />
        </ion-router>

        <ion-menu id="ion-menu" side="start" type="overlay" swipeGesture={false} content-id="menu-content">
          <ion-content>
            <ion-menu-toggle autoHide={false}>
              <app-menu></app-menu>

              <app-footer></app-footer>
            </ion-menu-toggle>
          </ion-content>
        </ion-menu>

        <ion-nav id="menu-content" />

        <app-share-deck ref={(el) => (this.shareRef = el as HTMLAppShareDeckElement)}></app-share-deck>
      </ion-app>,
    ];
  }
}
