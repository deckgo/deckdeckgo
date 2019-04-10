import {Component} from '@stencil/core';

@Component({
    tag: 'app-footer',
    styleUrl: 'app-footer.scss',
    shadow: false
})
export class AppFooter {

    render() {
        return [
            <hr margin/>,
            <div padding-start padding-end margin-bottom>
                <ion-anchor href="/about" routerDirection="forward">
                    <ion-label>About</ion-label>
                </ion-anchor>

                <ion-anchor href="/team" routerDirection="forward">
                    <ion-label>Team</ion-label>
                </ion-anchor>

                <ion-anchor href="/opensource" routerDirection="forward">
                    <ion-label>Open source</ion-label>
                </ion-anchor>

                <ion-anchor href="/privacy" routerDirection="forward">
                    <ion-label>Privacy Policy</ion-label>
                </ion-anchor>

                <ion-anchor href="/terms" routerDirection="forward">
                    <ion-label>Terms of use</ion-label>
                </ion-anchor>

                <ion-anchor href="/services" routerDirection="forward">
                    <ion-label>Services</ion-label>
                </ion-anchor>

                <ion-anchor href="/contact" routerDirection="forward">
                    <ion-label>Contact</ion-label>
                </ion-anchor>
            </div>
        ]
    }
}
