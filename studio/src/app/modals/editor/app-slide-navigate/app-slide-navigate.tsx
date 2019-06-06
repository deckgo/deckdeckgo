import {Component, Listen, Element, State, h} from '@stencil/core';

@Component({
    tag: 'app-slide-navigate',
    styleUrl: 'app-slide-navigate.scss'
})
export class AppSlideNavigate {

    @Element() el: HTMLElement;

    @State()
    slides: string[];

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        this.slides = await this.getSlidesTitle();
    }

    @Listen('popstate', { target: 'window' })
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private async jumpToSlide(index: number) {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(index);
    }

    private getSlidesTitle(): Promise<string[]> {
        return new Promise<string[]>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const results: string[] = [];

            const slides: NodeListOf<HTMLElement> = document.querySelectorAll('deckgo-deck > *');

            if (slides) {
                for (const slide of Array.from(slides)) {
                    if (slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                        const title: HTMLElement = slide.querySelector('[slot="title"]');

                        if (title) {
                            results.push(title.textContent);
                        } else {
                            const start: HTMLElement = slide.querySelector('[slot="start"]');

                            if (start) {
                                results.push(start.textContent);
                            } else {
                                const end: HTMLElement = slide.querySelector('[slot="end"]');

                                if (end) {
                                    results.push(end.textContent);
                                } else {
                                    results.push('');
                                }
                            }
                        }
                    }
                }
            }

            resolve(results);
        });
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
