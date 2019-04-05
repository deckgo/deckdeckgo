import {Component, State} from '@stencil/core';

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

    private switchType(e: CustomEvent) {
        this.type = DemoType[e.detail.value.toUpperCase()] as DemoType;
    }

    render() {
        return <div class="container">
            <ion-segment onIonChange={(e: CustomEvent) => this.switchType(e)}>
                <ion-segment-button value="mobile" checked={this.type === DemoType.MOBILE}>
                    <ion-label>Mobile</ion-label>
                </ion-segment-button>
                <ion-segment-button value="tablet" checked={this.type === DemoType.TABLET}>
                    <ion-label>Tablet</ion-label>
                </ion-segment-button>
                <ion-segment-button value="desktop" checked={this.type === DemoType.DESKTOP}>
                    <ion-label>Desktop</ion-label>
                </ion-segment-button>
                <ion-segment-button value="beamer" checked={this.type === DemoType.BEAMER}>
                    <ion-label>Beamer</ion-label>
                </ion-segment-button>
            </ion-segment>

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
            return <div class="beamer-container">
                <iframe src="https://www.deckdeckgo.com" />
            </div>
        } else {
            return undefined;
        }
    }
}
