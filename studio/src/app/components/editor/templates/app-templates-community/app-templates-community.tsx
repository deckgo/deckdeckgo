import {Component, Fragment, h, Event, EventEmitter} from '@stencil/core';

import templatesStore from '../../../../stores/templates.store';

import {Template} from '../../../../models/data/template';

@Component({
  tag: 'app-templates-community',
})
export class AppTemplatesCommunity {
  @Event()
  navigateTemplates: EventEmitter<void>;

  render() {
    if (templatesStore.state.user.length <= 0) {
      return (
        <Fragment>
          <ion-label>Share a template with the community. Follow this guide to get started.</ion-label>

          <ion-button
            class="ion-margin-top"
            shape="round"
            href="/templates"
            routerDirection="forward"
            mode="md"
            color="primary"
            onClick={() => this.navigateTemplates.emit()}>
            <ion-label>Share a template</ion-label>
          </ion-button>
        </Fragment>
      );
    }

    return templatesStore.state.user.map((_template: Template) => {
      return <div>TODO</div>;
    });
  }
}
