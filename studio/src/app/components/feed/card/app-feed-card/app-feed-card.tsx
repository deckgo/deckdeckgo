import {Component, Prop} from '@stencil/core';

@Component({
    tag: 'app-feed-card',
    styleUrl: 'app-feed-card.scss',
    shadow: false
})
export class AppFeedCard {

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

    @Prop()
    compact: boolean = true;

    @Prop()
    miniature: boolean = true;

    render() {

        return <ion-card class={this.editable ? "ion-no-margin" : undefined}>
            <app-feed-card-content editable={this.editable} author={this.author} publication={this.publication} caption={this.caption} description={this.description} compact={this.compact} miniature={this.miniature}></app-feed-card-content>
        </ion-card>
    }

}
