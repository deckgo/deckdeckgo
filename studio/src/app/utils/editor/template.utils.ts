import {Template, Slide, SlideScope} from '@deckdeckgo/editor';

import {Utils} from '../core/utils';

import templatesStore from '../../stores/templates.store';

export class TemplateUtils {
  static async loadScript(template: Template) {
    if (!template.data.cdn) {
      return;
    }

    await Utils.injectJS({
      id: `${template.data.tag}-script`,
      src: template.data.cdn,
      module: true
    });
  }

  static async loadSlideTemplate(slide: Slide) {
    const template: Template | undefined = await this.getTemplate(slide.data.scope, slide.data.template);

    if (!template) {
      return;
    }

    await this.loadScript(template);
  }

  static async getTemplate(scope: SlideScope, template: string): Promise<Template | undefined> {
    if (!scope || scope === SlideScope.DEFAULT) {
      return undefined;
    }

    const templates: Template[] = scope === SlideScope.COMMUNITY ? templatesStore.state.community : templatesStore.state.user;

    return templates.find((filteredTemplate: Template) => filteredTemplate.data.tag === template);
  }
}
