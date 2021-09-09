import {Build, Component, h, State} from '@stencil/core';

import {TimerService} from './services/timer/timer.service';
import {AccelerometerService} from './services/accelerometer/accelerometer.service';
import {ThemeService} from './services/theme/theme.service';

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.scss'
})
export class AppRoot {
  private timerService: TimerService;
  private accelerometerService: AccelerometerService;

  private themeService: ThemeService;

  @State()
  private loading: boolean = true;

  constructor() {
    this.timerService = TimerService.getInstance();
    this.accelerometerService = AccelerometerService.getInstance();
    this.themeService = ThemeService.getInstance();
  }

  async componentWillLoad() {
    await this.themeService.initDarkModePreference();
  }

  async componentDidLoad() {
    this.loading = false;

    await this.timerService.restart();

    if (Build.isBrowser) {
      await this.accelerometerService.init();
    }
  }

  async disconnectedCallback() {
    await this.timerService.destroy();
  }

  render() {
    return (
      <ion-app class={this.loading ? 'loading' : undefined}>
        <ion-router useHash={false}>
          <ion-route url="/" component="app-remote"></ion-route>
          <ion-route url="/remote" component="app-remote"></ion-route>
          <ion-route url="/remote/:room" component="app-remote"></ion-route>

          <ion-route url="/timer" component="app-timer"></ion-route>

          <ion-route url="/settings" component="app-settings"></ion-route>

          <ion-route url="/about" component="app-about"></ion-route>
        </ion-router>

        <ion-menu side="start" type="overlay" swipeGesture={false} content-id="menu-content">
          <ion-header>
            <ion-toolbar></ion-toolbar>
          </ion-header>

          <ion-content>
            <ion-menu-toggle autoHide={false}>
              <ion-list class="ion-margin-top">
                <ion-item detail={false} href="/" routerDirection="forward">
                  <ion-label class="ion-padding-start ion-padding-end ion-text-uppercase">
                    <ion-icon name="phone-portrait"></ion-icon> Remote
                  </ion-label>
                </ion-item>
                <ion-item detail={false} href="/timer" routerDirection="forward">
                  <ion-label class="ion-padding-start ion-padding-end ion-text-uppercase">
                    <ion-icon name="stopwatch"></ion-icon> Timer
                  </ion-label>
                </ion-item>
                <ion-item detail={false} href="/settings" routerDirection="forward">
                  <ion-label class="ion-padding-start ion-padding-end ion-text-uppercase">
                    <ion-icon name="settings"></ion-icon> Settings
                  </ion-label>
                </ion-item>
                <ion-item detail={false}>
                  <ion-label class="ion-padding-start ion-padding-end ion-text-uppercase">
                    <a href="https://deckdeckgo.com" target="_blank" rel="noopener" class="deckdeckgo">
                      <app-logo></app-logo>
                      <span>DeckDeckGo</span>
                    </a>
                  </ion-label>
                </ion-item>
              </ion-list>
            </ion-menu-toggle>
          </ion-content>
        </ion-menu>

        <ion-nav id="menu-content" />
      </ion-app>
    );
  }
}
