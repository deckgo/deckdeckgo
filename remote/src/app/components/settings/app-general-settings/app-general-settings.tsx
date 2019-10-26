import {Component, h, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {ThemeService} from '../../../services/theme/theme.service';

@Component({
    tag: 'app-general-settings'
})
export class AppGeneralSettings {

    private themeService: ThemeService;

    constructor() {
        this.themeService = ThemeService.getInstance();
    }

    componentWillLoad() {
        this.themeService.watch().pipe(take(1)).subscribe((dark: boolean) => {
            this.darkTheme = dark;
        });
    }

    async toggleTheme() {
        this.darkTheme = !this.darkTheme;
        await this.themeService.switch(this.darkTheme);
    }

    @State()
    private darkTheme: boolean;

    render() {
        return <ion-list class="ion-padding">
            <ion-list-header class="ion-padding-bottom ion-padding-top">
                <ion-label>Hello darkness my old friend</ion-label>
            </ion-list-header>

            <p>
                The remote control detects your OS default settings but if you wish, toggle to a lighter or darker theme 🔦
            </p>

            <ion-item>
                <ion-label>{this.darkTheme ? 'Dark theme' : 'Light theme'}</ion-label>
                <ion-toggle slot="end" checked={this.darkTheme} mode="md" color="switcher" onIonChange={() => this.toggleTheme()}></ion-toggle>
            </ion-item>
        </ion-list>;
    }

}
