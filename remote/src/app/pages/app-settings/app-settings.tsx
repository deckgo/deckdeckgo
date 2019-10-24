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
            <app-header></app-header>,

            <ion-content class="ion-padding">
                <ion-toggle checked={this.darkTheme} mode="md" color="switcher" onIonChange={() => this.toggleTheme()}></ion-toggle>
            </ion-content>
        ];
    }
}
