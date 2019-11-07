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
        return [
            <h1 class="ion-padding-top">Settings</h1>,
            <ion-list class="ion-padding-top ion-padding-bottom">
            <ion-item>
                <ion-label>{this.darkTheme ? 'Dark' : 'Light'} theme {this.darkTheme ? '🌑' : '☀️'}</ion-label>
                <ion-toggle slot="end" checked={this.darkTheme} mode="md" color="switcher" onIonChange={() => this.toggleTheme()}></ion-toggle>
            </ion-item>
        </ion-list>];
    }

}
