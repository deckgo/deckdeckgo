import {Component, Prop, State, Watch} from '@stencil/core';

import DateTimeFormatOptions = Intl.DateTimeFormatOptions;

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
    compact: boolean = true;

    @Prop()
    miniature: boolean = true;

    @Prop()
    editable: boolean = false;

    @Prop()
    author: string;

    @Prop()
    publication: Date;

    @Prop()
    caption: string;

    @Prop()
    description: string;

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
        return <ion-card-content class={this.compact ? "ion-no-padding compact" : "ion-no-padding"}>
            {this.renderMiniature()}

            <ion-card-header>
                <ion-card-title class="ion-text-uppercase" contentEditable={this.editable}>{this.caption}</ion-card-title>

                <ion-card-subtitle class="ion-text-lowercase">
                    {this.renderTags()}
                    {this.renderInputTags()}
                </ion-card-subtitle>
            </ion-card-header>

            <p padding-start padding-end class="content" contentEditable={this.editable}>{this.description}</p>

            <p class="author" padding>
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

    private renderMiniature() {
        if (this.miniature) {
            return <img src="/assets/dummy.png"/>;
        } else {
            return undefined;
        }
    }
}
