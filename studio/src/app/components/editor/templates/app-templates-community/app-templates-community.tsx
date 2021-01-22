import {Component, Fragment, h} from '@stencil/core';

import templatesStore from '../../../../stores/templates.store';

import {Template} from '../../../../models/data/template';

@Component({
  tag: 'app-templates-community',
})
export class AppTemplatesCommunity {
  render() {
    if (templatesStore.state.user.length <= 0) {
      return (
        <Fragment>
          <ion-label>Share a template with the community. Follow this guide to get started.</ion-label>
        </Fragment>
      );
    }

    return templatesStore.state.user.map((_template: Template) => {
      return <div>TODO</div>;
    });
  }
}
