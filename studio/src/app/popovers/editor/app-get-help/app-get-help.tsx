import {Component, Element, h} from '@stencil/core';

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
        <h2>Need help?</h2>
        <p>If you need help, have any feedback or ideas, we would love to hear from you 😃</p>
        <p>
          Reach us on{' '}
          <a
            href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
            target="_blank"
            rel="noopener noreferrer">
            Slack
          </a>
          , via <a href="mailto:hello@deckdeckgo.com">email</a> or on{' '}
          <a href="https://twitter.com/deckdeckgo" rel="noopener noreferrer">
            Twitter
          </a>
          .
        </p>
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            Got it
          </ion-button>
        </div>
      </div>
    );
  }
}
