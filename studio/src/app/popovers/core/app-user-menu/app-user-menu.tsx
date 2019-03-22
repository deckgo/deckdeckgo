import {Component, Element} from '@stencil/core';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
    tag: 'app-user-menu',
    styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {

    @Element() el: HTMLElement;

    private authService: AuthService;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    private async signOut() {
        await this.authService.logout();
        await this.closePopover();
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
