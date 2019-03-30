import {Component} from '@stencil/core';

@Component({
    tag: 'app-about',
    styleUrl: 'app-about.scss'
})
export class AppAbout {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content padding>

                <main padding>
                    <h1>About DeckDeckGo</h1>

                    <h3 padding-top>Edit anywhere, display everywhere</h3>

                    <p padding-top>DeckDeckGo aims to be the open source editor for PWA presentations.</p>

                    <p>What does that mean 🤔? It means that every presentations you write and publish with DeckDeckGo are also <strong>apps</strong> for desktop and mobile 🤪</p>

                    <p>Furthermore, DeckDeckGo aims to be an online community for sharing presentations, slides and talks about your interests and ideas too.</p>

                    <h1 class="ion-text-center ion-padding-top">Team</h1>

                    <div class="team">
                        <div>
                            <app-avatar src="https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg"></app-avatar>

                            <h2>David Dal Busco</h2>

                            <p text-center>David is a freelancer by day and the creator of DeckDeckGo by night. He's also the organiser of the Ionic Meetup Zürich and used to play in a band called VanRonMaiden, which was probably the coolest band ever but no one will ever know 🤣</p>

                            <div class="social-links">
                                <a href="http://github.com/peterpeterparker">
                                    <ion-icon name="logo-github" area-label="Github"></ion-icon>
                                </a>

                                <a href="https://twitter.com/daviddalbusco">
                                    <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                                </a>

                                <a href="https://medium.com/@david.dalbusco">
                                    <ion-icon src="./assets/img/medium-icon.svg" area-label="Medium"></ion-icon>
                                </a>
                            </div>
                        </div>

                        <div>
                            <app-avatar src="https://pbs.twimg.com/profile_images/588789970690658305/Ru9JiWks_400x400.png"></app-avatar>

                            <h2>Nicolas Mattia</h2>

                            <p class="ion-text-center">Nicolas ... has a bio as soon as he'll send me a PR 😉</p>

                            <div class="social-links">
                                <a href="https://github.com/nmattia">
                                    <ion-icon name="logo-github" area-label="Github"></ion-icon>
                                </a>

                                <a href="https://twitter.com/nasmattia">
                                    <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                                </a>

                                <a href="https://nmattia.com">
                                    <ion-icon name="globe" area-label="Personal website"></ion-icon>
                                </a>
                            </div>
                        </div>
                    </div>

                    <p padding-top>DeckDeckGo was created with passion in Zürich🇨🇭in 2019</p>

                </main>

            </ion-content>
        ];
    }

}
