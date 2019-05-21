import {Component, Element} from '@stencil/core';

import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {PhotoHistoryService} from '../../../services/editor/photo-history/photo-history.service';

@Component({
    tag: 'app-user-menu',
    styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {

    @Element() el: HTMLElement;

    private authService: AuthService;
    private navService: NavService;

    private photoHistoryService: PhotoHistoryService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
        this.photoHistoryService = PhotoHistoryService.getInstance();

    }

    private async signOut() {
        await this.authService.signOut();
        await this.photoHistoryService.clear();

        await this.closePopover();

        this.navService.navigate({
            url: '/',
            direction: NavDirection.ROOT
        });
    }

    async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    render() {
        return [
            <app-user-info></app-user-info>,
            this.renderActions()
        ]
    }

    private renderActions() {
        return <ion-list>
            <ion-item onClick={() => this.closePopover()}>
                <ion-anchor href="/settings" routerDirection="forward"><ion-label>Settings</ion-label></ion-anchor>
            </ion-item>

            <ion-item onClick={() => this.signOut()}>
                <ion-anchor><ion-label>Sign out</ion-label></ion-anchor>
            </ion-item>
        </ion-list>
    }

}
