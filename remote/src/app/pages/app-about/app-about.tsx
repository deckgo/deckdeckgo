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
                    <ion-title text-uppercase>DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,

            <ion-content padding>
                <h1 padding-bottom>DeckDeckGo</h1>

                <p>Create a PWA presentation using Web Components, <a href="http://ionicframework.com">Ionic</a> components and HTML or Markdown 🚀</p>

                <p>Cherry on the cake 🍒🎂 DeckDeckGo comes with this Progressive Web App that allows you to remote control your presentation through real time communication with <a href="https://webrtc.org">WebRTC</a> 🚀</p>

                <h2 padding-bottom padding-top>Getting Started</h2>

                <p>Start you new presentation by following the quick  👉 <a href="https://docs.deckdeckgo.com/docs">Getting Started guide</a> 👈</p>

                <h2 padding-bottom padding-top>Open Source</h2>

                <p>DeckDeckGo is <strong>open source</strong> and its source code could be found in the following repos:</p>

                <div text-center padding class="github-links">
                    <a href="https://github.com/deckgo/deckdeckgo"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo | the core</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-app"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-app | this progressive web app</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-docs"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-docs | the documentation</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-remote"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-remote | the remote controller</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-starter"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-starter | the starter kit</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-website"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-website | the website</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-charts"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-charts | to plot charts</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-highlight-code"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-highlight-code | highlight code</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-qrcode"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-qrcode | to create QR code</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-backend"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-backend | the signaling server</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-types"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-types | interfaces and enums</ion-badge></a>
                    <a href="https://github.com/deckgo/deckdeckgo-webpack-plugins"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> deckdeckgo-webpack-plugins | plugins</ion-badge></a>
                    <a href="https://github.com/deckgo/create-deckdeckgo"><ion-badge color="primary"><ion-icon name="logo-github"></ion-icon> create-deckdeckgo | the cli</ion-badge></a>
                </div>

                <div padding margin text-center class="social-links">
                    <a href="https://deckdeckgo.com">
                        <ion-icon name="globe"></ion-icon>
                    </a>
                    <a href="https://twitter.com/deckdeckgo">
                        <ion-icon name="logo-twitter"></ion-icon>
                    </a>
                </div>

                <p padding-top>Created by <a href="https://twitter.com/daviddalbusco">David Dal Busco</a> - v1.0.0-alpha.7</p>

            </ion-content>
        ];
    }
}
