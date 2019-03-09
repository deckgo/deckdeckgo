import {Component, State, Element} from '@stencil/core';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @State()
    private slides: any[] = [];

    componentWillLoad() {
        this.addSlide();
    }

    private addSlide() {
        if (!document) {
            return;
        }

        const title = <h1 slot="title" contenteditable>Click to add title</h1>;

        const content = <p slot="content" contenteditable>Click to add content</p>;

        const slide: any = <deckgo-slide-title>
            {title}
            {content}
        </deckgo-slide-title>;

        this.slides = [...this.slides, slide];
    }

    private async animatePrevNextSlide(next: boolean) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if (next) {
            await (deck as any).slideNext(false, false);
        } else {
            await (deck as any).slidePrev(false, false);
        }
    }

    // TODO: SlideTo

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content padding>
                <main>
                    <deckgo-deck embedded={true}>
                        {this.renderSlides()}
                    </deckgo-deck>
                </main>
            </ion-content>,
            <ion-footer>
                <ion-toolbar>
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.animatePrevNextSlide(false)} color="primary">
                            <ion-icon slot="icon-only" name="arrow-back"></ion-icon>
                        </ion-button>

                        <ion-button onClick={() => this.animatePrevNextSlide(true)} color="primary">
                            <ion-icon slot="icon-only" name="arrow-forward"></ion-icon>
                        </ion-button>

                        <ion-button onClick={() => this.addSlide()} color="primary">
                            <ion-icon slot="icon-only" name="bookmark"></ion-icon>
                        </ion-button>
                    </ion-buttons>

                    <ion-buttons slot="end">
                        <ion-button onClick={() => this.addSlide()} color="primary" shape="round" size="small">
                            <ion-label>Add slide</ion-label>
                        </ion-button>
                    </ion-buttons>
                </ion-toolbar>
            </ion-footer>
        ];
    }

    private renderSlides() {
        return (
            this.slides.map((slide: any) => {
                return slide
            })
        );
    }
}
