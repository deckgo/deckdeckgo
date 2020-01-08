import {Component, Element, h, Host} from '@stencil/core';

@Component({
    tag: 'app-landing',
    styleUrl: 'app-landing.scss',
    shadow: false
})
export class AppLanding {

    @Element() el: HTMLElement;

    private scrollToAudience() {
        const audienceSection: HTMLElement = this.el.querySelector('app-landing-content div.audience');

        if (audienceSection) {
            audienceSection.scrollIntoView({behavior: 'smooth'});
        }
    }

    render() {

        return <Host>
            <section class="header">
                <app-landing-deck onLearnMore={() => this.scrollToAudience()}></app-landing-deck>
            </section>

            <app-landing-content></app-landing-content>

            <app-landing-footer></app-landing-footer>
        </Host>
    }
}
