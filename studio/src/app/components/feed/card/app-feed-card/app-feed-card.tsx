import {Component, Prop, State, Watch} from '@stencil/core';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

@Component({
    tag: 'app-feed-card',
    styleUrl: 'app-feed-card.scss',
    shadow: false
})
export class AppFeedCard {

    @State()
    private tags: string[] = [];

    @Prop()
    author: string;

    @Prop()
    publication: Date;

    @Prop()
    caption: string;

    @Prop()
    description: string;

    @Prop()
    compact: boolean = true;

    @Prop()
    miniature: boolean = true;

    @State()
    private formattedPublication: string;

    async componentWillLoad() {
        await this.formatPublication();
    }

    @Watch('publication')
    formatPublication(): Promise<void> {
        return new Promise<void>((resolve) => {
            const options: DateTimeFormatOptions = {year: 'numeric', month: 'short', day: 'numeric'};
            this.formattedPublication = new Intl.DateTimeFormat('en-US', options).format(new Date());

            resolve();
        });
    }

    render() {
        return <ion-card>
            {this.renderCardContent()}
        </ion-card>
    }

    private renderCardContent() {
        return <ion-card-content class={this.compact ? "ion-no-padding compact" : "ion-no-padding"}>
            {this.renderMiniature()}

            <ion-card-header>
                <ion-card-title>{this.caption}</ion-card-title>

                <ion-card-subtitle class="ion-text-lowercase">
                    {this.renderTags()}
                </ion-card-subtitle>
            </ion-card-header>

            <p class="content ion-padding-start ion-padding-end">{this.description}</p>

            <p class="author ion-padding">
                <ion-label>{this.author} | {this.formattedPublication}</ion-label>
            </p>
        </ion-card-content>
    }

    private renderTags() {
        if (!this.tags || this.tags.length <= 0) {
            return undefined;
        } else {
            return (
                this.tags.map((tag: string) => {
                    return (
                        <div class="chips">
                            <ion-label>{tag}</ion-label>
                        </div>
                    )
                })
            );
        }
    }

    private renderMiniature() {
        if (this.miniature) {
            return <deckgo-lazy-img img-src="/assets/dummy.png"></deckgo-lazy-img>;
        } else {
            return undefined;
        }
    }

}
