import {Component} from '@stencil/core';

@Component({
    tag: 'app-developer',
    styleUrl: 'app-developer.scss'
})
export class AppDeveloper {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main padding>
                    <h1>Developer</h1>

                    <h2 class="ion-padding-top">You know what's cool?</h2>

                    <p>The core of DeckDeckGo and many other libraries we have developed could be used separately with or without in any modern framework. These are the exact same libraries we are using to develop this application respectively our platform. It means that each time we are going to improve it, the related open sourced library will inherit these improvements too 🚀</p>

                    <h3 class="ion-padding-top">Cherry on the cake 🍒🎂</h3>

                    <p>We hope that you do like to edit, present and publish your talk with our application. But if you are a developer and rather like to use <strong>Html</strong> or <strong>Markdown</strong> to compose your slides, we do provide a Cli, starter kit and even a full <a href="https://docs.deckdeckgo.com">documentation</a> 😁</p>

                    <p class="ion-margin-bottom ion-padding-bottom">If you would like to follow that path, run the following command in your terminal to begin your journey.</p>

                    <deckgo-highlight-code language="bash">
                        <code slot="code">npm init deckdeckgo</code>
                    </deckgo-highlight-code>
                </main>
            </ion-content>
        ];
    }

}

