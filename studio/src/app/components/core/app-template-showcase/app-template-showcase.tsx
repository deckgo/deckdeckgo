import {Component, Prop, h} from '@stencil/core';

import {Template} from '../../../models/data/template';

@Component({
  tag: 'app-template-showcase',
  styleUrl: 'app-template-showcase.scss',
})
export class AppTemplateShowcase {
  @Prop()
  template: Template;

  render() {
    return <article>yo yo</article>;
  }
}
