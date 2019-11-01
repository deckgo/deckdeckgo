import {Component, Element, h, Listen, Prop, State} from '@stencil/core';

@Component({
    tag: 'app-notes',
    styleUrl: 'app-notes.scss'
})
export class AppNotes {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @State()
    private notes: string;

    componentWillLoad() {
        if (this.selectedElement) {
            const element: HTMLElement = this.selectedElement.querySelector('[slot="notes"]');

            this.notes = element ? element.innerHTML : undefined;
        }
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('popstate', { target: 'window' })
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(false);
    }

    async save() {
        if (!this.selectedElement) {
            await this.closeModal();
            return;
        }

        if (this.notes === undefined || !this.notes || this.notes === '') {
            await this.closeModal();
            return;
        }

        const text: Text = document.createTextNode(this.notes);

        let element: HTMLElement = this.selectedElement.querySelector('[slot="notes"]');

        if (!element) {
            element = document.createElement('div');
            element.setAttribute('slot', 'notes');

            this.selectedElement.appendChild(element);

            element.appendChild(text);
        } else {
            element.replaceChild(text, element.firstChild);
        }

        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(true);
    }

    // TODO delete notes

    private handleNotesInput($event: CustomEvent<KeyboardEvent>) {
        this.notes = ($event.target as InputTargetEvent).value;
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="quaternary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Notes</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <p>Your notes are displayed in the <a href="https://deckdeckgo.app" target="_blank">remote control <ion-icon name="open"></ion-icon></a>.</p>

                <ion-list class="ion-no-padding">
                    <ion-item>
                        <ion-textarea rows={16} value={this.notes} debounce={500} class="ion-no-margin"
                                      maxlength={4096} placeholder="The notes related to the current slide"
                                      onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleNotesInput(e)}></ion-textarea>
                    </ion-item>
                </ion-list>

                <ion-button disabled={this.notes === undefined || !this.notes || this.notes === ''} color="dark" shape="round" onClick={() => this.save()} class="ion-margin-top">
                    <ion-label>Save</ion-label>
                </ion-button>
            </ion-content>
        ];
    }
}
