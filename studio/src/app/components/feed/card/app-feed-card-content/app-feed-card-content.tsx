import {Component, Prop} from '@stencil/core';

@Component({
    tag: 'app-feed-card-content',
    styleUrl: 'app-feed-card-content.scss',
    shadow: false
})
export class AppFeedCardContent {

    @Prop()
    firstCard: boolean = false;

    render() {
        return <ion-card-content>
            <div class="summary">

                <ion-card-header>
                    <ion-card-title class="ion-text-uppercase">Card Title</ion-card-title>

                    <ion-card-subtitle class="ion-text-lowercase">
                        <div class="chips"><ion-label>Javascript&nbsp;</ion-label></div>
                        <div class="chips"><ion-label>Typescript&nbsp;</ion-label></div>
                        <div class="chips"><ion-label>Ionic&nbsp;</ion-label></div>
                    </ion-card-subtitle>
                </ion-card-header>

                <p padding-start padding-end class="content ion-text-lowercase">Keep close to Nature's heart... and break clear away, once in
                    awhile,
                    and climb a mountain or spend a week in the woods. Wash your spirit clean.
                </p>

                <p class="author" padding>
                    <ion-label>David Dal Busco | Mars 9</ion-label>
                </p>
            </div>

            {this.renderPreviewImage()}
        </ion-card-content>
    }

    private renderPreviewImage() {
        if (this.firstCard) {
            return undefined;
        } else {
            return <div class="preview">
                <img src="./assets/img/deckdeckgo-logo.svg"/>
            </div>;
        }
    }

}
