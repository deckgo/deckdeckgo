import {Component, h, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {ThemeService} from '../../services/theme/theme.service';

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppSettings {

    private themeService: ThemeService;

    @State()
    private darkTheme: boolean;

    constructor() {
        this.themeService = ThemeService.getInstance();
    }

    componentWillLoad() {
        this.themeService.watch().pipe(take(1)).subscribe((dark: boolean) => {
            this.darkTheme = dark;

            console.log(dark);
        });
    }

    async toggleTheme() {
        this.darkTheme = !this.darkTheme;
        await this.themeService.switch(this.darkTheme);
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-menu-toggle>
                            <ion-button>
                                <ion-icon slot="icon-only" name="menu"></ion-icon>
                            </ion-button>
                        </ion-menu-toggle>
                    </ion-buttons>

                    <ion-title class="ion-text-uppercase">DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,

            <ion-content class="ion-padding">
                <ion-toggle checked={this.darkTheme} mode="md" color="switcher" onIonChange={() => this.toggleTheme()}></ion-toggle>
            </ion-content>
        ];
    }
}
