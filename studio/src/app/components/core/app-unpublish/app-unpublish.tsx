import {Component, h} from '@stencil/core';
import i18n from '../../../stores/i18n.store';
import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-unpublish',
  styleUrl: 'app-unpublish.scss'
})
export class AppUnpublish {
  render() {
    return (
      <p class="ion-padding-top">
        {renderI18n(i18n.state.settings.un_publish, {
          placeholder: '{0}',
          value: (
            <a href="https://deckdeckgo.com/en/contact/" rel="noopener norefferer" target="_blank">
              contact
            </a>
          )
        })}
      </p>
    );
  }
}
