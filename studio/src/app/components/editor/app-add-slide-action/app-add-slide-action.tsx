import {Component, Event, EventEmitter} from '@stencil/core';

@Component({
    tag: 'app-add-slide-action',
    styleUrl: 'app-add-slide-action.scss',
    shadow: false
})
export class AppAddSlideAction {

    @Event() private actionOpenSlideAdd: EventEmitter<UIEvent>;

    private openSlideAdd($event: UIEvent) {
        this.actionOpenSlideAdd.emit($event);
    }

    render() {
        return <ion-button onClick={(e: UIEvent) => this.openSlideAdd(e)} color="primary" shape="round"
                           size="small">
            <ion-label>Add slide</ion-label>
        </ion-button>;
    }

}
