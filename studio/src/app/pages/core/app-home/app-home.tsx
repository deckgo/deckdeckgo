import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-home',
})
export class AppHome {
  render() {
    return [<app-navigation presentation={true}></app-navigation>, this.renderContent()];
  }

  private renderContent() {
    return (
      <ion-content>
        <app-landing></app-landing>
      </ion-content>
    );
  }
}
