import {Component, Event, EventEmitter, h, State} from '@stencil/core';
import {Resources} from '../../../../utils/core/resources';
import {debounceTime, take} from 'rxjs/operators';
import {Deck} from '../../../../models/deck';
import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';
import {ApiDeckService} from '../../../../services/api/deck/api.deck.service';
import {ErrorService} from '../../../../services/core/error/error.service';
import {Subject, Subscription} from 'rxjs';

@Component({
    tag: 'app-publish-edit',
    styleUrl: 'app-publish-edit.scss'
})
export class AppPublishEdit {

    @State()
    private caption: string;

    @State()
    private description: string;

    @State()
    private valid: boolean = true;

    @State()
    private disablePublish: boolean = false;

    @State()
    private publishing: boolean = false;

    @State()
    private tag: string;

    @State()
    private tags: string[] = [];

    private deckEditorService: DeckEditorService;
    private deckService: ApiDeckService;

    private errorService: ErrorService;

    private updateDeckSubsction: Subscription;
    private updateDeckSubject: Subject<void> = new Subject();

    @Event() private published: EventEmitter<string>;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.deckService = ApiDeckService.getInstance();

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
        this.description = await this.getFirstSlideContent();
    }

    componentDidUnload() {
        if (this.updateDeckSubsction) {
            this.updateDeckSubsction.unsubscribe();
        }
    }

    private getFirstSlideContent(): Promise<string> {
        return new Promise<string>(async (resolve) => {
            let content: string = '';

            if (!document) {
                resolve('');
                return;
            }

            const slide: HTMLElement = document.querySelector('deckgo-deck > *:first-child');

            if (slide && slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

                if (contentElement) {
                    content = contentElement.textContent;
                }
            }

            resolve(content);
        });
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
                this.publishing = true;

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck || !deck.id) {
                        this.publishing = false;

                        resolve();
                        return;
                    }

                    const publishedUrl: string = await this.deckService.publish(deck);

                    // TODO: URL and Deck?
                    // For the time being url but in the future...
                    this.published.emit(publishedUrl);

                    this.publishing = false;

                    resolve();
                });
            } catch (err) {
                this.publishing = false;
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
        return <article>
            <h1>Share your presentation online</h1>

            <p><strong>Publish</strong> your presentation to share it with the world, your colleagues, friends and community.</p>

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
                        <ion-input value={this.caption} debounce={500} minlength={3} disabled={this.publishing}
                                   maxlength={Resources.Constants.DECK.TITLE_MAX_LENGTH} required={true}
                                   input-mode="text"
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onCaptionInput(e)}
                                   onIonChange={() => this.validateCaptionInput()}></ion-input>
                    </ion-item>

                    <ion-item class="item-title">
                        <ion-label>Description</ion-label>
                    </ion-item>

                    <ion-item>
                        <ion-textarea rows={5} value={this.description} disabled={this.publishing}></ion-textarea>
                    </ion-item>

                    <ion-item class="item-title">
                        <ion-label>Tags</ion-label>
                    </ion-item>

                    <ion-item>
                        <ion-input debounce={500} input-mode="text" value={this.tag} placeholder="Add a tag..."
                                   disabled={!this.tags || this.tags.length >= 5 || this.publishing}
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.onTagInput(e)}
                                   onIonChange={() => this.onTagChange()}></ion-input>
                    </ion-item>

                    <app-feed-card-tags tags={this.tags} editable={true} onRemoveTag={($event: CustomEvent) => this.removeTag($event)}></app-feed-card-tags>
                </ion-list>

                <div class="ion-padding ion-text-center publish">
                    {this.renderPublish()}
                </div>
            </form>

            <p class="social">No images need to be uploaded for the the social card of your presentation. DeckDeckGo will automatically generate it for you based on the first slide of your deck.</p>

        </article>
    }

    private renderPublish() {
        if (!this.publishing) {
            return <ion-button type="submit" disabled={!this.valid || this.disablePublish} color="tertiary"
                               shape="round">
                <ion-label>Publish now</ion-label>
            </ion-button>
        } else {
            return <div class="publishing">
                <ion-label>Publishing</ion-label>
                <ion-spinner name="dots" color="tertiary"></ion-spinner>
            </div>;
        }
    }
}
