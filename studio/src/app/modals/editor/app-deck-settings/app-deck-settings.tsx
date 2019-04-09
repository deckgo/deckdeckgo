import {Component, Listen, Element, State} from '@stencil/core';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';

@Component({
    tag: 'app-deck-settings',
    styleUrl: 'app-deck-settings.scss'
})
export class AppDeckSettings {

    @Element() el: HTMLElement;

    @State()
    private presentationName: string;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    componentWillLoad() {
        this.presentationName = this.deckEditorService.deck ? this.deckEditorService.deck.name : null;
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private presentationNameChange($event: CustomEvent) {
        if ($event && $event.detail && $event.detail.value) {
            this.presentationName = $event.detail.value;
        }
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Settings</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <ion-item>
                    <ion-label position="stacked">Presentation name</ion-label>
                    <ion-input value={this.presentationName} color="primary" debounce={300} inputmode="text" minlength={5} required={true}
                        onIonChange={(e: CustomEvent) => this.presentationNameChange(e)}></ion-input>
                </ion-item>

                <div class="ion-padding ion-text-center">
                    <ion-button shape="round" color="primary">
                        <ion-label class="ion-text-uppercase">Save</ion-label>
                    </ion-button>
                </div>
            </ion-content>
        ];
    }

}
