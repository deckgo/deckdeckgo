import {Build, Component, Element, h, State} from '@stencil/core';

import {toastController} from '@ionic/core';

import errorStore from './stores/error.store';
import navStore from './stores/nav.store';
import shareStore, {ShareData} from './stores/share.store';

import {AuthService} from './services/auth/auth.service';

import {ThemeService} from './services/theme/theme.service';
import {OfflineService} from './services/editor/offline/offline.service';
import {NavDirection, NavParams} from './stores/nav.store';

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.scss',
})
export class AppRoot {
  @Element() el: HTMLElement;

  private authService: AuthService;

  private themeService: ThemeService;

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
    this.offlineService = OfflineService.getInstance();
  }

  async componentWillLoad() {
    if (Build.isBrowser) {
      await this.authService.init();
      await this.themeService.initDarkModePreference();
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

  componentDidUnload() {
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

  private async toastError(error: string) {
    const popover: HTMLIonToastElement = await toastController.create({
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

    popover.onDidDismiss().then(() => {
      errorStore.state.error = undefined;
    });

    await popover.present();
  }

  private async navigate(params: NavParams) {
    if (!params) {
      return;
    }

    const router: HTMLIonRouterElement = this.el.querySelector('ion-router');

    if (!router) {
      return;
    }

    if (params.direction === NavDirection.ROOT) {
      window.location.assign(params.url);
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
          <ion-route url="/" component="app-welcome" />

          <ion-route url="/home" component="app-home" />

          <ion-route url="/discover" component="app-discover" />

          <ion-route url="/enterprise" component="app-enterprise" />

          <ion-route url="/editor" component="app-editor" />
          <ion-route url="/editor/:deckId" component="app-editor" />

          <ion-route url="/settings" component="app-settings" />

          <ion-route url="/dashboard" component="app-dashboard" />

          <ion-route url="/signin" component="app-signin" />
          <ion-route url="/signin/:redirect" component="app-signin" />

          <ion-route url="/about" component="app-about" />
          <ion-route url="/faq" component="app-faq" />
          <ion-route url="/team" component="app-team" />
          <ion-route url="/opensource" component="app-opensource" />
          <ion-route url="/privacy" component="app-privacy" />
          <ion-route url="/terms" component="app-terms" />
          <ion-route url="/services" component="app-services" />
          <ion-route url="/developer" component="app-developer" />
          <ion-route url="/contact" component="app-contact" />
          <ion-route url="/newsletter" component="app-newsletter" />
          <ion-route url="/press" component="app-press" />

          <ion-route url="/remote" component="app-remote" />

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
