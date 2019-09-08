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

        return <Host class={styleClass}>
            {this.renderDescription()}
            {this.renderHelp()}
        </Host>
    }

    private renderDescription() {
        if (this.description) {
            return [<h1 class="ion-padding ion-margin-start ion-margin-end">What the heck is DeckDeckGo?</h1>,
                <ion-card>
                    <ion-card-content>
                        <p>DeckDeckGo aims to be the <strong>open source</strong> web editor for <strong>presentations</strong>.</p>

                        <p class="ion-padding-top">What makes it different 🤔?</p>

                        <p class="ion-padding-top">Every presentations published with DeckDeckGo are standalone <strong>apps</strong> 🚀</p>

                        <p class="ion-padding-top">Moreover, it would be incredible if it would become an online community for sharing presentations, slides and talks about your interests and ideas.</p>

                        <p class="ion-padding-top ion-padding-bottom">Edit your deck anywhere, display it everywhere.</p>
                    </ion-card-content>
                </ion-card>];
        } else {
            return undefined;
        }
    }

    private renderHelp() {
        if (this.help) {
            return [<h1 class="ion-padding ion-margin-start ion-margin-end">We need your help!</h1>,
                <ion-card>
                    <ion-card-content>
                        <p>We need your help to improve DeckDeckGo 🙏</p>

                        <p class="ion-padding-top">Send us your feedback and ideas on <a href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNzM0NjMwOTc3NTI0LTBlNmFhODNhYmRkMWUxZmU4ZTQ2MDJiNjlmYWZiODNjMDU5OGRjYThlZmZjMTc5YmQ3MzUzMDlhMzk0ZDgzMDY" target="_blank">Slack</a>, via <a href="mailto:hello@deckdeckgo.com">email</a> or on <a href="https://twitter.com/deckdeckgo">Twitter</a>.</p>

                        <p class="ion-padding-top">You are awesome!</p>
                    </ion-card-content>
                </ion-card>]
        } else {
            return undefined;
        }
    }

}
