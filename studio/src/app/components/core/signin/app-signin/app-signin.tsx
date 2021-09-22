import {Component, State, h} from '@stencil/core';

import navStore, {NavDirection} from '../../../../stores/nav.store';
import i18n from '../../../../stores/i18n.store';

import {AppIcon} from '../../app-icon/app-icon';

import {EnvironmentAppConfig} from '../../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../../services/environment/environment-config.service';

@Component({
  tag: 'app-signin',
  styleUrl: 'app-signin.scss'
})
export class AppSignIn {
  @State()
  private signInInProgress: boolean = false;

  private appConfig: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  async navigateBack() {
    navStore.state.nav = {
      direction: NavDirection.BACK
    };
  }

  private updateInProgress = ({detail}: CustomEvent<boolean>) => {
    this.signInInProgress = detail;
  };

  render() {
    return [
      <main class="ion-padding fit">
        {this.renderBackButton()}

        {this.renderSignInMethod()}
      </main>
    ];
  }

  private renderSignInMethod() {
    const {cloud} = this.appConfig;

    if (cloud === 'firebase') {
      return <app-signin-firebase onInProgress={this.updateInProgress}></app-signin-firebase>;
    } else if (cloud === 'ic') {
      return <app-signin-ic onInProgress={this.updateInProgress}></app-signin-ic>;
    }

    return undefined;
  }

  private renderBackButton() {
    return (
      <ion-buttons class="back">
        <ion-button onClick={() => this.navigateBack()} color="dark" aria-label={i18n.state.core.close} disabled={this.signInInProgress}>
          <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
        </ion-button>
      </ion-buttons>
    );
  }
}
