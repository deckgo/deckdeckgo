import {Component, Element, h} from '@stencil/core';

import {SlideTemplate} from '../../../models/slide';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

@Component({
    tag: 'app-slide-type',
    styleUrl: 'app-slide-type.scss'
})
export class AppSlideType {

    @Element() el: HTMLElement;

    async componentDidLoad() {
        await this.lazyLoadContent();
    }

    private lazyLoadContent(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif');

            if (!slideGif) {
                resolve();
                return;
            }

            await (slideGif as any).lazyLoadContent();

            resolve();
        });
    }

    private async addSlide(template: SlideTemplate) {
        const slide: any = await CreateSlidesUtils.createSlide(template);
        await this.closePopover(template, slide);
    }

    async closePopover(template: SlideTemplate, slide?: any) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            slide: slide,
            template: template
        });
    }

    render() {
        return [<div class="ion-padding title">
                <h2>Add a slide</h2>
            </div>,
            <div class="container">
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
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.GIF)}>
                    <deckgo-slide-gif src={EnvironmentConfigService.getInstance().get('gifExampleSrc')} alt="Slide Gif">
                        <p slot="header" style={{"font-size": "var(--font-size-very-small)", padding: "2px", "border-radius": "4px"}}>Gif with header</p>
                        <p slot="footer" style={{"font-size": "var(--font-size-very-small)", padding: "2px", "border-radius": "4px"}}>and footer</p>
                    </deckgo-slide-gif>
                </div>
            </div>
        ];
    }

}
