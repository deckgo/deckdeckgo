import {initAuth, initSync} from '@deckdeckgo/sync';
import {toastController} from '@ionic/core';
import {Component, Element, h, Listen} from '@stencil/core';
import {BusyEvents} from './events/editor/busy/busy.events';
import {ErrorEvents} from './events/editor/error/error.events';
import {ColorService} from './services/editor/color/color.service';
import {EnvironmentConfigService} from './services/environment/environment-config.service';
import {LangService} from './services/lang/lang.service';
import {SettingsService} from './services/settings/settings.service';
import {ThemeService} from './services/theme/theme.service';
import navStore, {NavDirection, NavParams} from './stores/nav.store';
import {authConfig} from './utils/core/auth.utils';

@Component({
  tag: 'app-root'
})
export class AppRoot {
  @Element() el: HTMLElement;

  private readonly themeService: ThemeService;
  private readonly colorService: ColorService;
  private readonly settingsService: SettingsService;
  private readonly langService: LangService;

  private destroyErrorListener;
  private destroyNavListener;
  private destroySyncListeners: (() => void)[];

  private readonly errorEvents: ErrorEvents = new ErrorEvents();
  private readonly busyEvents: BusyEvents = new BusyEvents();

  constructor() {
    this.themeService = ThemeService.getInstance();
    this.colorService = ColorService.getInstance();
    this.settingsService = SettingsService.getInstance();
    this.langService = LangService.getInstance();
  }

  async componentWillLoad() {
    this.destroySyncListeners = initSync({
      env: {
        cloud: EnvironmentConfigService.getInstance().get('cloud'),
        jszip: EnvironmentConfigService.getInstance().get('jszip')
      }
    });

    const promises: Promise<void>[] = [
      initAuth(authConfig()),
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
    this.errorEvents.init();
    this.busyEvents.init();

    this.destroyNavListener = navStore.onChange('nav', async (params: NavParams | undefined) => {
      await this.navigate(params);
    });
  }

  disconnectedCallback() {
    this.errorEvents.destroy();
    this.busyEvents.destroy();

    this.destroyErrorListener?.();
    this.destroyNavListener?.();
    this.destroySyncListeners?.forEach((unsubscribe: () => void) => unsubscribe());
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

  /**
   * Note: Routes need to be flat as we path the return and deckId (redirect) to the signin route. So no /settings/something but /something.
   */
  render() {
    return [
      <ion-app>
        <ion-router useHash={false}>
          <ion-route url="/" component="app-editor" />

          <ion-route url="/profile" component="app-profile" />
          <ion-route url="/customization" component="app-customization" />
          <ion-route url="/templates" component="app-templates" />

          <ion-route url="/decks" component="app-decks" />
          <ion-route url="/docs" component="app-docs" />

          <ion-route url="/storage" component="app-storage" />

          <ion-route url="/signin" component="app-signin-page" />

          <ion-route url="/poll" component="app-poll" />
          <ion-route url="/poll/:pollKey" component="app-poll" />

          <ion-route url=":pathname" component="app-404" />
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

        <app-share></app-share>
      </ion-app>
    ];
  }
}
