import {Component, h, Fragment} from '@stencil/core';

@Component({
  tag: 'app-dashboard-page',
})
export class AppDashboardPage {
  render() {
    return [
      <Fragment>
        <app-navigation presentation={true}></app-navigation>
        <ion-content class="ion-padding">
          <app-dashboard></app-dashboard>
        </ion-content>
      </Fragment>,
    ];
  }
}
