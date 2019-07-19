import {Component, Element, h, Listen, Prop, State} from '@stencil/core';
@Component({
    tag: 'app-youtube',
    styleUrl: 'app-youtube.scss'
})
export class AppYoutube {

    @Element() el: HTMLElement;

    @State()
    private youtubeUrl: string;

    @Prop()
    selectedElement: HTMLElement;

    componentWillLoad() {
        if (this.selectedElement) {
            this.youtubeUrl = this.selectedElement.getAttribute('src');
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
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    async save() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(this.youtubeUrl);
    }

    private handleInput($event: CustomEvent<KeyboardEvent>) {
        this.youtubeUrl = ($event.target as InputTargetEvent).value;
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="youtube">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">YouTube</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <ion-list>
                    <ion-item>
                        <ion-input value={this.youtubeUrl} placeholder="Enter the URL of the video" debounce={500}
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}></ion-input>
                    </ion-item>
                </ion-list>

                <ion-button disabled={this.youtubeUrl === undefined || !this.youtubeUrl || this.youtubeUrl === ''} color="dark" shape="round" onClick={() => this.save()}>
                    <ion-label>Save</ion-label>
                </ion-button>
            </ion-content>
        ];
    }

}
