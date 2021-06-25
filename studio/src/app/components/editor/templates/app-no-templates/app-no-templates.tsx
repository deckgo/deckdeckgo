import {Component, h} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {renderI18n} from '../../../../utils/core/i18n.utils';

@Component({
  tag: 'app-no-templates',
  styleUrl: 'app-no-templates.scss'
})
export class AppNoTemplates {
  render() {
    return (
      <ion-label>
        {renderI18n(i18n.state.templates.no_personal_templates, {
          placeholder: '{0}',
          value: (
            <a href="https://github.com/deckgo/template-kit" rel="noopener norefferer" target="_blank" class="tutorial">
              {i18n.state.templates.no_personal_templates_guide}
            </a>
          )
        })}
      </ion-label>
    );
  }
}
