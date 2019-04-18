import {Component} from '@stencil/core';

@Component({
    tag: 'app-about',
    styleUrl: 'app-about.scss'
})
export class AppAbout {

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-title class="ion-text-uppercase">DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,

            <ion-content padding>
                <h1 class="ion-padding-bottom">DeckDeckGo</h1>

                <p>Create a PWA presentation using Web Components, <a href="http://ionicframework.com">Ionic</a> components and HTML or Markdown 🚀</p>

                <p>Cherry on the cake 🍒🎂 DeckDeckGo comes with this Progressive Web App that allows you to remote control your presentation through real time communication with <a href="https://webrtc.org">WebRTC</a> 🚀</p>

                <h2 class="ion-padding-bottom ion-padding-top">Getting Started</h2>

                <p>Start you new presentation by following the quick  👉 <a href="https://docs.deckdeckgo.com/docs">Getting Started guide</a> 👈</p>

                <h2 class="ion-padding-bottom ion-padding-top">Open Source</h2>

                <p>DeckDeckGo is <strong>open source</strong> and its source code could be found on <a href="https://github.com/deckgo/deckdeckgo">Github <ion-icon name="logo-github"></ion-icon></a></p>

                <div class="social-links ion-padding ion-margin ion-text-center">
                    <a href="https://deckdeckgo.com">
                        <ion-icon name="globe"></ion-icon>
                    </a>
                    <a href="https://twitter.com/deckdeckgo">
                        <ion-icon name="logo-twitter"></ion-icon>
                    </a>
                </div>

                <p class="ion-padding-top">DeckDeckGo is developed by <a href="https://twitter.com/daviddalbusco">David Dal Busco</a> and <a href="https://twitter.com/nasmattia">Nicolas Mattia</a> - v1.0.0-alpha.8</p>

            </ion-content>
        ];
    }
}
