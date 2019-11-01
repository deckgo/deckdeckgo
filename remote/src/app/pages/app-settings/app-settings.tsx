import {Component, h, State} from '@stencil/core';

enum SettingsTab {
    GENERAL= 'general',
    EXPERIMENTAL = 'experimental'
}

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppSettings {

    @State()
    private tab: SettingsTab = SettingsTab.GENERAL;

    private selectTab($event: CustomEvent) {
        if ($event && $event.detail) {
            this.tab = $event.detail.value;
        }
    }

    render() {
        return [
            <app-header></app-header>,

            <ion-content class="ion-padding">
                <ion-segment mode="md" color="switcher" onIonChange={($event: CustomEvent) => this.selectTab($event)}>
                    <ion-segment-button mode="md" value={SettingsTab.GENERAL} checked={this.tab === SettingsTab.GENERAL}>
                        <ion-label>General</ion-label>
                    </ion-segment-button>
                    <ion-segment-button mode="md" value={SettingsTab.EXPERIMENTAL} checked={this.tab === SettingsTab.EXPERIMENTAL}>
                        <ion-label>Experimental</ion-label>
                    </ion-segment-button>
                </ion-segment>

                {this.renderTab()}
            </ion-content>
        ];
    }

    private renderTab() {
        if (this.tab === SettingsTab.EXPERIMENTAL) {
            return <app-experimental-settings></app-experimental-settings>;
        } else {
            return <app-general-settings></app-general-settings>;
        }
    }
}
