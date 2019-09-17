import {Component, h} from '@stencil/core';

@Component({
    tag: 'app-team',
    styleUrl: 'app-team.scss'
})
export class AppTeam {

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                <main class="ion-padding">
                    <h1>Team</h1>

                    <div class="team">
                        <div>
                            <app-avatar src="https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg" aria-label="David Dal Busco"></app-avatar>

                            <h2>David Dal Busco</h2>

                            <p text-center>David is a freelancer by day and the creator of DeckDeckGo by night. He's also the organiser of the Ionic Meetup Zürich and used to play in a band called VanRonMaiden, which was probably the coolest band ever but no one will ever know 🤣</p>

                            <div class="social-links">
                                <a href="https://twitter.com/daviddalbusco">
                                    <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                                </a>

                                <a href="https://daviddalbusco.com">
                                    <ion-icon name="globe" area-label="Personal blog and website"></ion-icon>
                                </a>

                                <a href="https://dev.to/daviddalbusco">
                                    <ion-icon src="./assets/icons/dev.svg" area-label="Dev"></ion-icon>
                                </a>

                                <a href="https://medium.com/@david.dalbusco">
                                    <ion-icon src="./assets/icons/medium.svg" area-label="Medium"></ion-icon>
                                </a>

                                <a href="http://github.com/peterpeterparker">
                                    <ion-icon name="logo-github" area-label="Github"></ion-icon>
                                </a>
                            </div>
                        </div>

                        <div>
                            <app-avatar src="https://pbs.twimg.com/profile_images/588789970690658305/Ru9JiWks_400x400.png" arial-label="Nicolas Mattia"></app-avatar>

                            <h2>Nicolas Mattia</h2>

                            <p class="ion-text-center">Nicolas ... has a bio as soon as he'll send me a PR 😉</p>

                            <div class="social-links">
                                <a href="https://twitter.com/nasmattia">
                                    <ion-icon name="logo-twitter" area-label="Twitter"></ion-icon>
                                </a>

                                <a href="https://nmattia.com">
                                    <ion-icon name="globe" area-label="Personal blog and website"></ion-icon>
                                </a>

                                <a href="https://github.com/nmattia">
                                    <ion-icon name="logo-github" area-label="Github"></ion-icon>
                                </a>
                            </div>
                        </div>
                    </div>

                </main>

            </ion-content>
        ];
    }

}
