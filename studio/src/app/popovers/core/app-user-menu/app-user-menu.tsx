import {Component, Element} from '@stencil/core';

import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

@Component({
    tag: 'app-user-menu',
    styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {

    @Element() el: HTMLElement;

    private authService: AuthService;
    private navService: NavService;

    private imageHistoryService: ImageHistoryService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
        this.imageHistoryService = ImageHistoryService.getInstance();

    }

    private async signOut() {
        await this.authService.signOut();
        await this.imageHistoryService.clear();

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
