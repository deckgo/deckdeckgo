import {Component, Prop, State, Watch} from '@stencil/core';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

interface InputTargetEvent extends EventTarget {
    value: string;
}

@Component({
    tag: 'app-feed-card-content',
    styleUrl: 'app-feed-card-content.scss',
    shadow: false
})
export class AppFeedCardContent {

    @State()
    private tags: string[] = [];

    @State()
    private tagInput: string = null;

    @Prop()
    firstCard: boolean = false;

    @Prop()
    editable: boolean = false;

    @Prop()
    author: string;

    @Prop()
    publication: Date;

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

    private handleTagInput($event: UIEvent) {
        const tag: string = ($event.target as InputTargetEvent).value;

        if (tag && tag.trim().length > 0 && tag.charAt(tag.length - 1) === ' ' && this.tags && this.tags.indexOf(tag.trim()) === -1) {
            this.tags = [...this.tags, tag.trim()];
            this.tagInput = null;
        }
    }

    private removeTag(tag: string): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!tag) {
                resolve();
                return;
            }

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
        return <ion-card-content>
            <div class="summary">

                <ion-card-header>
                    <ion-card-title class="ion-text-uppercase" contentEditable={this.editable}>Card Title
                    </ion-card-title>

                    <ion-card-subtitle class="ion-text-lowercase">
                        {this.renderTags()}
                        {this.renderInputTags()}
                    </ion-card-subtitle>
                </ion-card-header>

                <p padding-start padding-end class="content" contentEditable={this.editable}>Keep
                    close to Nature's heart... and break clear away, once in
                    awhile,
                    and climb a mountain or spend a week in the woods. Wash your spirit clean.
                </p>

                <p class="author" padding>
                    <ion-label>{this.author} | {this.formattedPublication}</ion-label>
                </p>
            </div>

            {this.renderPreviewImage()}
        </ion-card-content>
    }

    private renderPreviewImage() {
        if (this.firstCard) {
            return undefined;
        } else {
            return <div class={this.editable ? "preview editable" : "preview"}>
                <img src="./assets/img/deckdeckgo-logo.svg"/>
            </div>;
        }
    }

    private renderTags() {
        if (!this.tags || this.tags.length <= 0) {
            return undefined;
        } else {
            return (
                this.tags.map((tag: string) => {
                    return (
                        <div class="chips">
                            {this.renderCloseTags(tag)}
                            <ion-label>{tag}</ion-label>
                        </div>
                    )
                })
            );
        }
    }

    private renderCloseTags(tag: string) {
        if (!this.editable) {
            return undefined;
        } else {
            <ion-icon name="close" custom-tappable onClick={() => this.removeTag(tag)}></ion-icon>
        }
    }

    private renderInputTags() {
        if (!this.editable) {
            return undefined;
        }

        if (this.tags && this.tags.length < 5) {
            return <input autofocus placeholder="Add a tag..." value={this.tagInput}
                          onInput={($event: UIEvent) => this.handleTagInput($event)}></input>
        } else {
            return undefined;
        }
    }
}
