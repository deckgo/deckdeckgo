import {Component, EventEmitter, Prop, Event, h} from '@stencil/core';


@Component({
    tag: 'app-feed-card-tags',
    styleUrl: 'app-feed-card-tags.scss',
    shadow: false
})
export class AppFeedCardTags {

    @Prop()
    tags: string[] = [];

    @Prop()
    editable: boolean = false;

    @Event() private removeTag: EventEmitter<string>;

    private remove(tag: string) {
        this.removeTag.emit(tag);
    }

    render() {
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
        if (this.editable) {
            return <ion-icon name="close" custom-tappable onClick={() => this.remove(tag)}></ion-icon>
        } else {
            return undefined;
        }
    }

}
