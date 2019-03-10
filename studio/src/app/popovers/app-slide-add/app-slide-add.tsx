import {Component, Element} from '@stencil/core';

@Component({
    tag: 'app-slide-add',
    styleUrl: 'app-slide-add.scss'
})
export class AppSlideAdd {

    @Element() el: HTMLElement;

    render() {
        return <div class="container">
            <div class="item">
                <deckgo-slide-title>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-title>
            </div>
            <div class="item">
                <deckgo-slide-content>
                    <h1 slot="title">Title</h1>
                    <p slot="content">Content</p>
                </deckgo-slide-content>
            </div>
            <div class="item">
                <deckgo-slide-split>
                    <h1 slot="title">Title</h1>
                    <p slot="start">Content</p>
                    <p slot="end">Content</p>
                </deckgo-slide-split>
            </div>
        </div>;
    }

}
