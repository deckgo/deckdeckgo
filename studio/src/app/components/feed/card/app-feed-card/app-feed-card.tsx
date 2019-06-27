import {Component, Prop, State, h} from '@stencil/core';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

import {Deck} from '../../../../models/data/deck';

@Component({
    tag: 'app-feed-card',
    styleUrl: 'app-feed-card.scss',
    shadow: false
})
export class AppFeedCard {

    @Prop()
    compact: boolean = true;

    @Prop()
    deck: Deck;

    @State()
    private caption: string;

    @State()
    private description: string;

    @State()
    private author: string;

    @State()
    private formattedPublishedAt: string;

    @State()
    private tags: string[] = [];

    async componentWillLoad() {
        await this.init();
    }

    private init(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.deck || !this.deck.data || !this.deck.data.meta) {
                resolve();
                return;
            }

            this.caption = this.deck.data.meta.title;
            this.description = this.deck.data.meta.description ? (this.deck.data.meta.description as string) : undefined;
            this.tags = this.deck.data.meta.tags as string[];

            await this.formatPublication();

            resolve();
        });
    }

    private formatPublication(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.deck.data.meta.published_at) {
                resolve();
                return;
            }

            const options: DateTimeFormatOptions = {year: 'numeric', month: 'short', day: 'numeric'};
            this.formattedPublishedAt = new Intl.DateTimeFormat('en-US', options).format(this.getDateObj(this.deck.data.meta.published_at));

            resolve();
        });
    }

    private getDateObj(myDate: any): Date {
        if (myDate == null) {
            return null;
        }

        if (myDate instanceof String || typeof myDate === 'string') {
            return new Date('' + myDate);
        }

        // A Firebase Timestamp format
        if (myDate && (myDate.seconds >= 0 || myDate.seconds < 0) && (myDate.nanoseconds >= 0 || myDate.nanoseconds < 0)) {
            return new Date(myDate.toDate());
        }

        return myDate;
    }

    render() {
        return <ion-card>
            {this.renderCardContent()}
        </ion-card>
    }

    private renderCardContent() {
        return <ion-card-content class={this.compact ? "ion-no-padding compact" : "ion-no-padding"}>
            <deckgo-lazy-img img-src="/assets/dummy.png"></deckgo-lazy-img>

            <ion-card-header>
                <ion-card-title>{this.caption}</ion-card-title>

                <app-feed-card-tags tags={this.tags}></app-feed-card-tags>
            </ion-card-header>

            <p class="content ion-padding-start ion-padding-end">{this.description}</p>

            <p class="author ion-padding">
                <ion-label>{this.author} | {this.formattedPublishedAt}</ion-label>
            </p>
        </ion-card-content>
    }

}
