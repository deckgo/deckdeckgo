import {Component, Element} from '@stencil/core';

@Component({
    tag: 'app-feed-lazy-img',
    styleUrl: 'app-feed-lazy-img.scss',
    shadow: true
})
export class AppFeedLazyImg {

    @Element() el: HTMLElement;

    private observer: IntersectionObserver;

    async componentDidLoad() {
        const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

        if (img) {
            this.observer = new IntersectionObserver(this.onIntersection, { rootMargin: '100px 0px' });
            this.observer.observe(img);
        }
    }

    private onIntersection = async (entries: IntersectionObserverEntry[]) => {
        for (const entry of entries) {
            if (entry.isIntersecting) {

                if (this.observer) {
                    this.observer.disconnect();
                }

                if (entry.target.getAttribute('data-src')) {
                    entry.target.setAttribute('src', entry.target.getAttribute('data-src'));
                    entry.target.removeAttribute('data-src');
                }
            }
        }
    };

    render() {
        return <img data-src="/assets/dummy.png"/>;
    }
}
