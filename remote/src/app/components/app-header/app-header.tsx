import {Component, h, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {ThemeService} from '../../services/theme/theme.service';

@Component({
    tag: 'app-header',
    styleUrl: 'app-header.scss'
})
export class AppHeader {

    private themeSubscription: Subscription;
    private themeService: ThemeService;

    @State()
    private darkTheme: boolean;

    constructor() {
        this.themeService = ThemeService.getInstance();
    }

    async componentWillLoad() {
        this.themeSubscription = this.themeService.watch().subscribe((dark: boolean) => {
            this.darkTheme = dark;
        });
    }

    componentWillUnload() {
        if (this.themeSubscription) {
            this.themeSubscription.unsubscribe();
        }
    }

    render() {
        return <ion-header>
            <ion-toolbar color={this.darkTheme ? 'dark' : 'primary'}>
                <slot name="start">
                    <ion-buttons slot="start">
                        <ion-menu-toggle>
                            <ion-button>
                                <ion-icon slot="icon-only" name="menu"></ion-icon>
                            </ion-button>
                        </ion-menu-toggle>
                    </ion-buttons>
                </slot>

                <slot name="end"></slot>
            </ion-toolbar>
        </ion-header>;
    }

}
