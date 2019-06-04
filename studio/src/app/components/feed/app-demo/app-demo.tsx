import {Component, State, h} from '@stencil/core';

enum DemoType {
    MOBILE = 'mobile',
    TABLET = 'tablet',
    DESKTOP = 'dekstop',
    BEAMER = 'beamer'
}

@Component({
    tag: 'app-demo',
    styleUrl: 'app-demo.scss',
    shadow: true
})
export class AppDemo {

    @State()
    private type: DemoType = DemoType.MOBILE;

    private switchType(type: DemoType) {
        this.type = type;
    }

    render() {
        return <div class="container">
            <div class="type-selection">
                <ion-anchor class={this.type === DemoType.MOBILE ? 'selected' : ''} onClick={() => this.switchType(DemoType.MOBILE)}>
                    <ion-label>Mobile</ion-label>
                </ion-anchor>
                <ion-anchor class={this.type === DemoType.TABLET ? 'selected' : ''} onClick={() => this.switchType(DemoType.TABLET)}>
                    <ion-label>Tablet</ion-label>
                </ion-anchor>
                <ion-anchor class={this.type === DemoType.DESKTOP ? 'selected' : ''} onClick={() => this.switchType(DemoType.DESKTOP)}>
                    <ion-label>Desktop</ion-label>
                </ion-anchor>
                <ion-anchor class={this.type === DemoType.BEAMER ? 'selected' : ''} onClick={() => this.switchType(DemoType.BEAMER)}>
                    <ion-label>Beamer</ion-label>
                </ion-anchor>
            </div>

            {this.renderMobile()}
            {this.renderTablet()}
            {this.renderDesktop()}
            {this.renderBeamer()}
        </div>;
    }

    private renderMobile() {
        if (this.type === DemoType.MOBILE) {
            return <div class="smartphone-container">
                <div class="smartphone">
                    <div class="content">
                        <iframe src="https://www.deckdeckgo.com" />
                    </div>
                </div>
            </div>
        } else {
            return undefined;
        }
    }

    private renderDesktop() {
        if (this.type === DemoType.DESKTOP) {
            return <div class="laptop-container">
                <div class="laptop">
                    <div class="content">
                        <iframe src="https://www.deckdeckgo.com" />
                    </div>
                </div>
            </div>
        } else {
            return undefined;
        }
    }

    private renderTablet() {
        if (this.type === DemoType.TABLET) {
            return <div class="tablet-container">
                <div class="tablet">
                    <div class="content">
                        <iframe src="https://www.deckdeckgo.com" />
                    </div>
                </div>
            </div>
        } else {
            return undefined;
        }
    }

    private renderBeamer() {
        if (this.type === DemoType.BEAMER) {
            return <div class="beamer-container ion-padding">
                <iframe src="https://www.deckdeckgo.com" />
            </div>
        } else {
            return undefined;
        }
    }
}
