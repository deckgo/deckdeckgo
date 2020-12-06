import {Component, Prop, h} from '@stencil/core';

import {Template, TemplateDataSlot} from '../../../models/data/template';

import {Utils} from '../../../utils/core/utils';

@Component({
  tag: 'app-template-showcase',
  styleUrl: 'app-template-showcase.scss',
})
export class AppTemplateShowcase {
  @Prop()
  template: Template;

  async componentWillLoad() {
    await Utils.injectJS({
      id: `${this.template.data.tag}-script`,
      src: this.template.data.cdn,
      module: true,
    });
  }

  render() {
    const Element = this.template.data.tag;
    return (
      <article>
        <Element>
          {this.template.data.slots.map((slot: TemplateDataSlot) => {
            return <h1 slot={slot.name}>Hello</h1>;
          })}
        </Element>
      </article>
    );
  }
}
