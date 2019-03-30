import {Component} from '@stencil/core';

@Component({
    tag: 'app-popular',
    styleUrl: 'app-popular.scss',
    shadow: false
})
export class AppPopular {

    // TODO: For the time being, display what's DeckDeckGo. In the future present dynamic content, like the popular or trending presentations

    render() {
        return [
            <h1 padding>What the heck is DeckDeckGo?</h1>,
            <ion-card>
                <ion-card-content>
                    <p>DeckDeckGo aims to be the <strong>open source</strong> editor for <strong>PWA presentations</strong>.</p>

                    <p padding-top>What does that mean 🤔?</p>

                    <p padding-top>It means that every presentations you write and publish with DeckDeckGo are also <strong>apps</strong> for desktop and mobile 🤪</p>

                    <p padding-top>Furthermore, DeckDeckGo aims to be an online community for sharing presentations, slides and talks about your interests and ideas too.</p>

                    <p padding-top><strong>Edit anywhere, display everywhere</strong></p>
                </ion-card-content>
            </ion-card>

        ];
    }

}
