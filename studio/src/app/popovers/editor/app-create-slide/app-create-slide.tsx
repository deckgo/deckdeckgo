import {Component, Element, Event, EventEmitter, h, JSX, State} from '@stencil/core';
import {take} from 'rxjs/operators';

import {SlideTemplate} from '../../../models/data/slide';

import {User} from '../../../models/data/user';
import {Deck} from '../../../models/data/deck';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {UserService} from '../../../services/data/user/user.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-create-slide',
    styleUrl: 'app-create-slide.scss'
})
export class AppCreateSlide {

    @Element() el: HTMLElement;

    @State()
    private photoUrl: string;

    @State()
    private chartsCollapsed: boolean = true;

    private user: User;

    private userService: UserService;
    private anonymousService: AnonymousService;
    private deckEditorService: DeckEditorService;

    @Event() signIn: EventEmitter<void>;

    constructor() {
        this.userService = UserService.getInstance();
        this.anonymousService = AnonymousService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
    }

    async componentWillLoad() {
        this.userService.watch().pipe(
            take(1)).subscribe(async (user: User) => {
            this.user = user;
            this.photoUrl = user && user.data && user.data.photo_url ? user.data.photo_url : 'https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg';
        });
    }

    async componentDidLoad() {
        await this.lazyLoadContent();
        await this.drawChart();
    }

    private lazyLoadContent(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif.showcase');
            const slideAuthor: HTMLElement = this.el.querySelector('deckgo-slide-author.showcase');
            const slideQRCode: HTMLElement = this.el.querySelector('deckgo-slide-qrcode.showcase');

            const slides: HTMLElement[] = [slideGif, slideAuthor, slideQRCode];

            if (!slides || slides.length <= 0) {
                resolve();
                return;
            }

            const promises = [];
            Array.from(slides).forEach((slide: HTMLElement) => {
                promises.push((slide as any).lazyLoadContent());
            });

            await Promise.all(promises);

            resolve();
        });
    }

    private drawChart(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slideChart: HTMLElement = this.el.querySelector('deckgo-slide-chart.showcase');

            if (!slideChart) {
                resolve();
                return;
            }

            await (slideChart as any).draw();

            resolve();
        });
    }

    private async addSlide(template: SlideTemplate, deck?: Deck) {
        const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide(template, deck, this.user);
        await this.closePopover(template, slide);
    }

    private addSlideQRCode() {
        this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
            await this.addSlide(SlideTemplate.QRCODE, deck);
        });
    }

    // We need the data in the user account (like twitter, profile image etc.) to generate the author slide
    private async addRestrictedSlide(template: SlideTemplate) {
        const isAnonymous: boolean = await this.anonymousService.isAnonymous();

        if (isAnonymous) {
            this.signIn.emit();
            await this.closePopover(null);
            return;
        }

        const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide(template, null, this.user);
        await this.closePopover(template, slide);
    }

    private async closePopoverWithoutResults() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async closePopover(template: SlideTemplate, slide?: JSX.IntrinsicElements) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            slide: slide,
            template: template
        });
    }

    render() {
        return [<ion-toolbar>
            <h2>Add a slide</h2>
            <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <div class="container ion-margin-bottom">
                <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.TITLE)}>
                    <deckgo-slide-title class="showcase">
                        <p slot="title">Title</p>
                        <p slot="content">Content</p>
                    </deckgo-slide-title>
                </div>
                <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.CONTENT)}>
                    <deckgo-slide-content class="showcase">
                        <p slot="title">Title</p>
                        <p slot="content">Content</p>
                    </deckgo-slide-content>
                </div>
                <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.SPLIT)}>
                    <deckgo-slide-split class="showcase">
                        <p slot="start">Content</p>
                        <p slot="end">Content</p>
                    </deckgo-slide-split>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.GIF)}>
                    <deckgo-slide-gif class="showcase" src={EnvironmentConfigService.getInstance().get('gifExampleSrc')}
                                      alt="Slide Gif">
                        <p slot="header" style={{
                            "font-size": "var(--font-size-very-small)",
                            padding: "2px",
                            "border-radius": "4px"
                        }}>Gif with header</p>
                        <p slot="footer" style={{
                            "font-size": "var(--font-size-very-small)",
                            padding: "2px",
                            "border-radius": "4px"
                        }}>and footer</p>
                    </deckgo-slide-gif>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.YOUTUBE)}>
                    <deckgo-slide-content class="showcase gif">
                        <p slot="title">Youtube</p>
                        <p slot="content">
                            <ion-icon name="logo-youtube"></ion-icon>
                        </p>
                    </deckgo-slide-content>
                </div>
                <div class="item" custom-tappable onClick={() => this.chartsCollapsed = !this.chartsCollapsed}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>

                {this.renderCharts()}

                <div class="item" custom-tappable onClick={() => this.addSlideQRCode()}>
                    <deckgo-slide-qrcode class="showcase" content="https://deckdeckgo.com" img-src="https://deckdeckgo.com/assets/img/deckdeckgo-logo.svg">
                        <p slot="title">QR code</p>
                    </deckgo-slide-qrcode>
                </div>
                <div class="item" custom-tappable onClick={() => this.addRestrictedSlide(SlideTemplate.AUTHOR)}>
                    <deckgo-slide-author class="showcase"
                                         img-src={this.photoUrl}
                                         img-alt="Author">
                        <p slot="title">Author</p>
                        <p slot="author">About yourself</p>
                        <p slot="social-link">Twitter</p>
                        <p slot="social-link">LinkedIn</p>
                        <p slot="social-link">Dev</p>
                        <p slot="social-link">Medium</p>
                        <p slot="social-link">Github</p>
                        <p slot="social-link">Web</p>
                    </deckgo-slide-author>
                </div>
            </div>
        ];
    }

    private renderCharts() {
        const chartsClass: string = `expand-charts ${this.chartsCollapsed ? 'collapsed' : ''}`;

        return <div class={chartsClass}>

            <div class="arrow"></div>

            <div class="list">
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.CHART)}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.CHART)}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.CHART)}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.CHART)}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>
                <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.CHART)}>
                    <deckgo-slide-chart class="showcase" type="line" y-axis-domain="extent" date-pattern="dd.MM.yyyy"
                                        marginTop={0} marginBottom={0} marginLeft={0} marginRight={0}
                                        width={204} height={68}
                                        src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
                        <p slot="title">Chart</p>
                    </deckgo-slide-chart>
                </div>
            </div>
        </div>;
    }

}
