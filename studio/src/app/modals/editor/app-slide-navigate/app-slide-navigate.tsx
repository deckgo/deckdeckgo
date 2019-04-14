import {Component, Listen, Element, Prop} from '@stencil/core';

@Component({
    tag: 'app-slide-navigate',
    styleUrl: 'app-slide-navigate.scss'
})
export class AppSlideNavigate {

    @Element() el: HTMLElement;

    @Prop()
    slides: string[];

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

    private async jumpToSlide(index: number) {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(index);
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
                    <ion-title class="ion-text-uppercase">Jump to slide</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <ion-list>
                    {this.renderSlides()}
                </ion-list>
            </ion-content>
        ];
    }

    private renderSlides() {
        if (this.slides && this.slides.length > 0) {
            return (
                this.slides.map((slideTitle: string, i: number) => {

                    const text = 'Slide ' + (i + 1) + (slideTitle ? ': ' + slideTitle : '');

                    return <ion-item ion-item button onClick={() => this.jumpToSlide(i)}>
                        <ion-label>{text}</ion-label>
                    </ion-item>
                })
            );
        } else {
            return undefined;
        }
    }
}
