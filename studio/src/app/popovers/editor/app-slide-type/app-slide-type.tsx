import {Component, Element, Event, EventEmitter, h, State} from '@stencil/core';
import {take} from 'rxjs/operators';

import {SlideTemplate} from '../../../models/data/slide';

import {User} from '../../../models/data/user';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {UserService} from '../../../services/data/user/user.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';

@Component({
    tag: 'app-slide-type',
    styleUrl: 'app-slide-type.scss'
})
export class AppSlideType {

    @Element() el: HTMLElement;

    @State()
    private photoUrl: string;

    private user: User;

    private userService: UserService;
    private anonymousService: AnonymousService;

    @Event() signIn: EventEmitter<void>;

    constructor() {
        this.userService = UserService.getInstance();
        this.anonymousService = AnonymousService.getInstance();
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
    }

    private lazyLoadContent(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif.showcase');
            const slideAuthor: HTMLElement = this.el.querySelector('deckgo-slide-author.showcase');

            const slides: HTMLElement[] = [slideGif, slideAuthor];

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

    private async addSlide(template: SlideTemplate) {
        const isAnonymous: boolean = await this.anonymousService.isAnonymous();

        if (isAnonymous) {
            this.signIn.emit();
            await this.closePopover(null);
            return;
        }

        const slide: any = await CreateSlidesUtils.createSlide(template, this.user);
        await this.closePopover(template, slide);
    }

    private async closePopoverWithoutResults() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async closePopover(template: SlideTemplate, slide?: any) {
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
                        <h1 slot="title">Title</h1>
                        <p slot="content">Content</p>
                    </deckgo-slide-title>
                </div>
                <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.CONTENT)}>
                    <deckgo-slide-content class="showcase">
                        <h1 slot="title">Title</h1>
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
                <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.AUTHOR)}>
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

}
