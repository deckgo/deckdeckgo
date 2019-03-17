import {Component} from '@stencil/core';

@Component({
    tag: 'app-contact',
    styleUrl: 'app-contact.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content padding>

                <main padding>
                    <h1>Contact</h1>

                    <p>We would love to hear from you, ping us 😃</p>

                    <p>Email: <a href="mailto:hello@deckdeckgo.com">hello@deckdeckgo.com</a></p>

                    <p>Twitter: <a href="https://twitter.com/deckdeckgo">@deckdeckgo</a></p>

                    <p>And for feature requests, issues or even better Pull Requests, find us on <a href="https://github.com/deckgo/deckdeckgo">GitHub</a></p>
                </main>
            </ion-content>
        ];
    }

}

