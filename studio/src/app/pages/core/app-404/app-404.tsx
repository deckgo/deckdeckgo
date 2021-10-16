import {Component, h, Fragment} from '@stencil/core';

import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';

import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-404',
  styleUrl: 'app-404.scss'
})
export class App404 {
  private navToEditor() {
    navStore.state.nav = {
      url: '/',
      direction: NavDirection.FORWARD
    };
  }

  render() {
    return (
      <Fragment>
        <app-navigation></app-navigation>
        <ion-content class="ion-padding">
          <main class="ion-padding fit">
            <h1>{i18n.state.not_found.title}</h1>

            {this.renderText()}

            <app-random-gif keyword="404"></app-random-gif>
          </main>
        </ion-content>
      </Fragment>
    );
  }

  private renderText() {
    return renderI18n(i18n.state.not_found.text, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => this.navToEditor()}>
          {i18n.state.not_found.action}
        </button>
      )
    });
  }
}
