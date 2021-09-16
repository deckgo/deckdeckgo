import {Component, Event, EventEmitter, h} from '@stencil/core';

import {Template} from '@deckdeckgo/editor';

import templatesStore from '../../../../stores/templates.store';

@Component({
  tag: 'app-templates-community'
})
export class AppTemplatesCommunity {
  @Event()
  selectedTemplate: EventEmitter<Template>;

  render() {
    return templatesStore.state.community.map((template: Template) => {
      return (
        <app-template-showcase
          template={template}
          author={true}
          key={template.id}
          custom-tappable
          onClick={() => this.selectedTemplate.emit(template)}></app-template-showcase>
      );
    });
  }
}
