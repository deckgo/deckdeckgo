import {Component, Element} from '@stencil/core';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/nav/nav.service';

@Component({
    tag: 'app-user-menu',
    styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {

    @Element() el: HTMLElement;

    private authService: AuthService;
    private navService: NavService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
    }

    private async signOut() {
        await this.authService.logout();
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
        return <div padding>
            <a onClick={() => this.signOut()}><p>Sign out</p></a>
        </div>
    }

}
