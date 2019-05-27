import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime, take} from 'rxjs/operators';

import {Deck} from '../../../models/deck';

import {Resources} from '../../../utils/core/resources';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {DeckService} from '../../../services/api/deck/deck.service';
import {ErrorService} from '../../../services/core/error/error.service';

@Component({
    tag: 'app-publish',
    styleUrl: 'app-publish.scss'
})
export class AppPublish {

    @Element() el: HTMLElement;

    @Prop()
    description: string;

    @State()
    private caption: string;

    @State()
    private valid: boolean = true;

    @State()
    private disablePublish: boolean = false;

    @State()
    private tag: string;

    @State()
    private tags: string[] = [];

    private deckEditorService: DeckEditorService;
    private deckService: DeckService;

    private errorService: ErrorService;

    private updateDeckSubsction: Subscription;
    private updateDeckSubject: Subject<void> = new Subject();

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
    }

    async componentWillLoad() {
        this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
            if (deck) {
                this.caption = deck.name;
            }
        });

        this.updateDeckSubsction = this.updateDeckSubject.asObservable().pipe(debounceTime(500)).subscribe(async () => {
            await this.updateDeck();
        });
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    componentDidUnload() {
        if (this.updateDeckSubsction) {
            this.updateDeckSubsction.unsubscribe();
        }
    }

    @Listen('window:popstate')
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private updateDeck(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.caption || this.caption === undefined || this.caption === '') {
                resolve();
                return;
            }

            this.disablePublish = true;

            try {
                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck || !deck.id) {
                        this.disablePublish = false;
                        resolve();
                        return;
                    }

                    deck.name = this.caption;

                    const updatedDeck: Deck = await this.deckService.put(deck);
                    this.deckEditorService.next(updatedDeck);

                    this.disablePublish = false;
                });
            } catch (err) {
                this.disablePublish = false;
                this.errorService.error(err);
            }

            resolve();
        });
    }

    private async handleSubmit(e: Event) {
        e.preventDefault();

        await this.publish();
    }

    private publish(): Promise<void> {
        return new Promise<void>((resolve) => {
            try {
                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck || !deck.id) {
                        resolve();
                        return;
                    }

                    await this.deckService.publish(deck);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                resolve();
            }
        });
    }

    private onCaptionInput($event: CustomEvent<KeyboardEvent>): Promise<void> {
        return new Promise<void>((resolve) => {
            let title: string = ($event.target as InputTargetEvent).value;
            if (title && title !== undefined && title !== '') {
                if (!this.validCaption(title)) {
                    title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
                }
            }

            this.caption = title;

            this.updateDeckSubject.next();

            resolve();
        });
    }

    private validateCaptionInput() {
        this.valid = this.validCaption(this.caption);
    }

    private validCaption(title: string): boolean {
        return title && title !== undefined && title !== '' && title.length < Resources.Constants.DECK.TITLE_MAX_LENGTH;
    }

    private onTagInput($event: CustomEvent<KeyboardEvent>): Promise<void> {
        return new Promise<void>((resolve) => {
            this.tag = ($event.target as InputTargetEvent).value;

            resolve();
        });
    }

    private onTagChange() {
        if (this.tag && this.tag !== undefined && this.tag !== null && this.tag.length  > 3) {
            if (this.tag.charAt(0) === '#') {
                this.tag = this.tag.substr(1);
            }

            if (this.tags && this.tags.indexOf(this.tag) === -1) {
                this.tags = [...this.tags, this.tag.trim()];
                this.tag = null;
            }
        }
    }

    private removeTag($event: CustomEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            const tag: string = $event.detail;

            if (!this.tags) {
                resolve();
                return;
            }

            const index: number = this.tags.findIndex((actualTag: string) => {
                return tag === actualTag
            });

            if (index >= 0) {
                this.tags.splice(index, 1);
                this.tags = [...this.tags];
            }

            resolve();
        });
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="tertiary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Ready to publish?</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding fullscreen-padding">
                <main padding>
                    <h1>Publish</h1>

                    <p>Publish your presentation to <strong>share</strong> it with the world, your colleagues, friends
                        and community.</p>

                    <p>DeckDeckGo will distribute it online as a modern <strong>app</strong>.</p>

                    <h2>Meta</h2>

                    <p class="meta-text">But first, edit or review your presentation's title and summary and add or change tags (up to 5) to
                        make your presentation more inviting to readers.</p>

                    <form onSubmit={(e: Event) => this.handleSubmit(e)}>
                        <ion-list class="inputs-list">
                            <ion-item class="item-title">
                                <ion-label>Title</ion-label>
                            </ion-item>

                            <ion-item>
                                <ion-input value={this.caption} debounce={500} minlength={3}
                                           maxlength={Resources.Constants.DECK.TITLE_MAX_LENGTH} required={true}
                                           input-mode="text"
                                           onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onCaptionInput(e)}
                                           onIonChange={() => this.validateCaptionInput()}></ion-input>
                            </ion-item>

                            <ion-item class="item-title">
                                <ion-label>Description</ion-label>
                            </ion-item>

                            <ion-item>
                                <ion-textarea rows={5} value={this.description}></ion-textarea>
                            </ion-item>

                            <ion-item class="item-title">
                                <ion-label>Tags</ion-label>
                            </ion-item>

                            <ion-item>
                                <ion-input debounce={500} input-mode="text" value={this.tag} placeholder="Add a tag..."
                                           disabled={!this.tags || this.tags.length >= 5}
                                           onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onTagInput(e)}
                                           onIonChange={() => this.onTagChange()}></ion-input>
                            </ion-item>

                            <app-feed-card-tags tags={this.tags} editable={true} onRemoveTag={($event: CustomEvent) => this.removeTag($event)}></app-feed-card-tags>
                        </ion-list>

                        <div class="ion-padding ion-text-center">
                            <ion-button type="submit" disabled={!this.valid || this.disablePublish} color="tertiary"
                                        shape="round">
                                <ion-label>Publish now</ion-label>
                            </ion-button>
                        </div>
                    </form>

                    <p class="social">No images need to be uploaded for the the social card of your presentation. DeckDeckGo will automatically generate it for you based on the first slide of your deck.</p>

                </main>
            </ion-content>
        ];
    }

}
