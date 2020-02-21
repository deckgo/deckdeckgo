import {Component, Event, EventEmitter, Prop, State, h} from '@stencil/core';

import {popoverController} from '@ionic/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth/auth.user';
import {User} from '../../../models/data/user';

import {Utils} from '../../../utils/core/utils';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {UserService} from '../../../services/data/user/user.service';
import {ThemeService} from '../../../services/theme/theme.service';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Prop() signIn: boolean = true;
  @Prop() presentation: boolean = false;
  @Prop() publish: boolean = false;

  private authService: AuthService;
  private subscription: Subscription;

  private navService: NavService;

  private userService: UserService;
  private userSubscription: Subscription;

  private themeService: ThemeService;
  private themeSubscription: Subscription;

  @State()
  private authUser: AuthUser;

  @State()
  private photoUrl: string;

  @State()
  private photoUrlLoaded: boolean = false;

  @State()
  private darkMode: boolean;

  @Event() private actionPublish: EventEmitter<void>;

  constructor() {
    this.authService = AuthService.getInstance();
    this.navService = NavService.getInstance();
    this.userService = UserService.getInstance();
    this.themeService = ThemeService.getInstance();
  }

  componentWillLoad() {
    this.subscription = this.authService.watch().subscribe((authUser: AuthUser) => {
      this.authUser = authUser;
    });

    this.userSubscription = this.userService.watch().subscribe((user: User) => {
      this.photoUrl = user && user.data ? user.data.photo_url : undefined;
      this.photoUrlLoaded = true;
    });

    this.themeSubscription = this.themeService.watch().subscribe((dark: boolean) => {
      this.darkMode = dark;
    });
  }

  componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }

    if (this.userSubscription) {
      this.userSubscription.unsubscribe();
    }

    if (this.themeSubscription) {
      this.themeSubscription.unsubscribe();
    }
  }

  private async openMenu($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-user-menu',
      event: $event,
      mode: 'ios'
    });

    await popover.present();
  }

  private async navigateSignIn() {
    this.navService.navigate({
      url: '/signin' + (window && window.location ? window.location.pathname : ''),
      direction: NavDirection.FORWARD
    });
  }

  render() {
    return (
      <div>
        {this.renderFeed()}
        {this.renderSignIn()}
        {this.renderPresentationButton()}
        {this.renderPublishButton()}
        {this.renderLoggedIn()}
      </div>
    );
  }

  private renderFeed() {
    if (Utils.isLoggedIn(this.authUser) || !this.signIn) {
      return undefined;
    } else if (this.presentation || this.publish) {
      return (
        <ion-router-link href="/discover" routerDirection="forward" class="wide-device ion-padding-start ion-padding-end">
          <ion-label>Discover</ion-label>
        </ion-router-link>
      );
    }
  }

  private renderSignIn() {
    if (Utils.isLoggedIn(this.authUser) || !this.signIn) {
      return undefined;
    } else if (this.presentation || this.publish) {
      return (
        <a class="wide-device ion-padding-start ion-padding-end signin" onClick={() => this.navigateSignIn()} tabindex={0}>
          <ion-label>Sign in</ion-label>
        </a>
      );
    }
  }

  private renderLoggedIn() {
    if (Utils.isLoggedIn(this.authUser) && this.photoUrlLoaded) {
      return (
        <a class="ion-padding-end" onClick={(e: UIEvent) => this.openMenu(e)} aria-label="Open menu" tabindex={0}>
          <app-avatar src={this.photoUrl}></app-avatar>
        </a>
      );
    } else {
      return undefined;
    }
  }

  private renderPresentationButton() {
    if (this.presentation) {
      return (
        <ion-button class="presentation ion-margin-end" shape="round" href="/editor" routerDirection="root" mode="md" color={this.darkMode ? 'light' : 'dark'}>
          <ion-label>Write a presentation</ion-label>
        </ion-button>
      );
    } else {
      return null;
    }
  }

  private renderPublishButton() {
    if (this.publish) {
      return (
        <ion-button class="publish ion-margin-end" shape="round" onClick={() => this.actionPublish.emit()} mode="md" color={this.darkMode ? 'light' : 'dark'}>
          <ion-label>Ready to share?</ion-label>
        </ion-button>
      );
    } else {
      return null;
    }
  }
}
