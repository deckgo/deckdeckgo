import {Component, Element} from '@stencil/core';

import {SlideTemplate} from '../../../models/slide-template';
import {EditorUtils} from '../../../utils/editor-utils';

@Component({
    tag: 'app-slide-type',
    styleUrl: 'app-slide-type.scss'
})
export class AppSlideType {

    @Element() el: HTMLElement;

    private async addSlide(template: SlideTemplate) {
        const slide: any = await EditorUtils.createSlide(template);
        await this.closePopover(slide);
    }

    async closePopover(slide: any) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            slide: slide
        });
    }

    render() {
        return <div class="container">
            <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.TITLE)}>
                <deckgo-slide-title>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-title>
            </div>
            <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.CONTENT)}>
                <deckgo-slide-content>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-content>
            </div>
            <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.SPLIT)}>
                <deckgo-slide-split>
                    <p slot="start">Content</p>
                    <p slot="end">Content</p>
                </deckgo-slide-split>
            </div>
        </div>;
    }

}
