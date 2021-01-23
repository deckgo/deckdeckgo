import {Utils} from '../core/utils';

import {Template} from '../../models/data/template';
import {Slide, SlideScope} from '../../models/data/slide';

import templatesStore from '../../stores/templates.store';

export class TemplateUtils {
  static async loadScript(template: Template) {
    if (!template.data.cdn) {
      return;
    }

    await Utils.injectJS({
      id: `${template.data.tag}-script`,
      src: template.data.cdn,
      module: true,
    });
  }

  static async loadSlideTemplate(slide: Slide) {
    if (!slide.data.scope || slide.data.scope === SlideScope.DEFAULT) {
      return;
    }

    const templates: Template[] = slide.data.scope === SlideScope.COMMUNITY ? templatesStore.state.community : templatesStore.state.user;

    const template: Template | undefined = templates.find((userTemplate: Template) => userTemplate.data.tag === slide.data.template);

    if (!template) {
      return;
    }

    await this.loadScript(template);
  }
}
