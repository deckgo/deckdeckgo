import {Component, Element} from '@stencil/core';
import {DeckdeckgoSlideTemplate} from '../../utils/deckdeckgo-slide-template';
import {DeckdeckgoStudioUtils} from '../../utils/deckdeckgo-studio-utils';

@Component({
    tag: 'app-slide-add',
    styleUrl: 'app-slide-add.scss'
})
export class AppSlideAdd {

    @Element() el: HTMLElement;

    private async addSlide(template: DeckdeckgoSlideTemplate) {
        const slide: any = await DeckdeckgoStudioUtils.createSlide(template);
        await this.closePopover(slide);
    }

    async closePopover(slide: any) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss(slide);
    }

    render() {
        return <div class="container">
            <div class="item" custom-tappable onClick={() => this.addSlide(DeckdeckgoSlideTemplate.TITLE)}>
                <deckgo-slide-title>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-title>
            </div>
            <div class="item" custom-tappable onClick={() => this.addSlide(DeckdeckgoSlideTemplate.CONTENT)}>
                <deckgo-slide-content>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-content>
            </div>
            <div class="item" custom-tappable onClick={() => this.addSlide(DeckdeckgoSlideTemplate.SPLIT)}>
                <deckgo-slide-split>
                    <h1 slot="title">Title</h1>
                    <p slot="start">Content</p>
                    <p slot="end">Content</p>
                </deckgo-slide-split>
            </div>
        </div>;
    }

}
