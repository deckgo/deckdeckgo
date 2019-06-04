import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-popular',
    styleUrl: 'app-popular.scss',
    shadow: false
})
export class AppPopular {

    // TODO: For the time being, display what's DeckDeckGo. In the future present dynamic content, like the popular or trending presentations

    render() {
        return [
            <h1 class="ion-padding ion-margin-start ion-margin-end">What the heck is DeckDeckGo?</h1>,
            <ion-card>
                <ion-card-content>
                    <p>DeckDeckGo aims to be the <strong>open source</strong> web editor for <strong>presentations</strong>.</p>

                    <p class="ion-padding-top">What makes it different 🤔?</p>

                    <p class="ion-padding-top">Every presentations published with DeckDeckGo are standalone <strong>apps</strong> 🚀</p>

                    <p class="ion-padding-top">Moreover, it is also an online community for sharing presentations, slides and talks about your interests and ideas.</p>

                    <p class="ion-padding-top ion-padding-bottom">Edit your deck anywhere, display it everywhere.</p>
                </ion-card-content>

                <app-demo></app-demo>
            </ion-card>,
            <h1 class="ion-padding ion-margin-start ion-margin-end">We need your help!</h1>,
            <ion-card>
                <ion-card-content>
                    <p>We are in development, we need your help.</p>

                    <p class="ion-padding-top">Send us your feedback via <a href="mailto:hello@deckdeckgo.com">email</a>, <a href="https://twitter.com/deckdeckgo">Twitter</a> or on <a href="https://join.slack.com/t/deckdeckgo/shared_invite/enQtNjMyNTk2NTQwODk5LTAxZjAwZWQwODQyZDg1ZDA5ODhlOTE3OGMwZjhmYjY3NDRhZjViZTRiNWU3OGU3MjYyNjE1OWE3NzNkZmQ3ZWI" target="_blank">Slack</a>.</p>

                    <p class="ion-padding-top">You are awesome!</p>
                </ion-card-content>
            </ion-card>
        ];
    }

}
