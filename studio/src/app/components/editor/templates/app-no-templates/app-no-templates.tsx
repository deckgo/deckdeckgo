import {Component, h} from '@stencil/core';

@Component({
  tag: 'app-no-templates',
})
export class AppNoTemplates {
  render() {
    return <ion-label>You do not have any personal templates yet. Follow this guide to get started and, add your first template afterwards.</ion-label>;
  }
}
