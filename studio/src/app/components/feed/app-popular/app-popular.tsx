import {Component, h, Prop, Host} from '@stencil/core';

@Component({
  tag: 'app-popular',
  styleUrl: 'app-popular.scss',
  shadow: false
})
export class AppPopular {
  @Prop()
  description: boolean = false;

  @Prop()
  help: boolean = false;

  // TODO: For the time being, display what's DeckDeckGo. In the future present dynamic content, like the popular or trending presentations

  render() {
    const helpStyleClass: string = this.help ? 'help' : undefined;
    const styleClass: string = this.description ? `description ${helpStyleClass}` : helpStyleClass;

    return (
      <Host class={styleClass}>
        {this.renderDescription()}
        {this.renderHelp()}
      </Host>
    );
  }

  private renderDescription() {
    if (this.description) {
      return [
        <h1 class="ion-padding ion-margin-start ion-margin-end">Featured presentations</h1>,
        <ion-card>
          <ion-card-content>
            <p>These decks were made with DeckDeckGo.</p>

            <p class="ion-padding-top">
              Each of them are standalone <strong>Progressive Web Apps</strong> 🚀
            </p>

            <p class="ion-padding-top">
              We aim to improve this feed in the future with some filters (tags, periodicity, etc.) and maybe even make it cleverer to personalize your feed for
              your interests and ideas.
            </p>
          </ion-card-content>
        </ion-card>
      ];
    } else {
      return undefined;
    }
  }

  private renderHelp() {
    if (this.help) {
      return [
        <h1 class="ion-padding ion-margin-start ion-margin-end">We need your help!</h1>,
        <ion-card>
          <ion-card-content>
            <p>We need your help to improve DeckDeckGo 🙏</p>

            <p class="ion-padding-top">
              Send us your feedback and ideas on{' '}
              <a
                href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY"
                target="_blank">
                Slack
              </a>
              , via <a href="mailto:hello@deckdeckgo.com">email</a> or on <a href="https://twitter.com/deckdeckgo">Twitter</a>.
            </p>

            <p class="ion-padding-top">You are awesome!</p>
          </ion-card-content>
        </ion-card>
      ];
    } else {
      return undefined;
    }
  }
}
