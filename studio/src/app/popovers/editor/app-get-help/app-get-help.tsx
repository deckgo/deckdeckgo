import {Component, Element, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-get-help',
  styleUrl: 'app-get-help.scss',
})
export class AppGetHelp {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <h2>{i18n.state.editor.need_help}</h2>
        <p>{i18n.state.editor.need_help_comment}</p>
        <p>
          {renderI18n(i18n.state.editor.reach_us, {
            placeholder: '{0}',
            value: (
              <div class="links">
                <a
                  href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
                  target="_blank"
                  rel="noopener noreferrer">
                  Slack
                </a>
                , <a href="mailto:hello@deckdeckgo.com">{i18n.state.settings.email}</a> {i18n.state.editor.or}{' '}
                <a href="https://twitter.com/deckdeckgo" rel="noopener noreferrer">
                  Twitter
                </a>
              </div>
            ),
          })}
        </p>
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            {i18n.state.editor.got_it}
          </ion-button>
        </div>
      </div>
    );
  }
}
