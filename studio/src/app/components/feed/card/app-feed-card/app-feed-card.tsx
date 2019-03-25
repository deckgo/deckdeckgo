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

    render() {

        return <ion-card class={this.editable ? "ion-no-margin" : undefined}>
            <app-feed-card-content editable={this.editable} author={this.author} publication={this.publication}></app-feed-card-content>
        </ion-card>
    }

}
