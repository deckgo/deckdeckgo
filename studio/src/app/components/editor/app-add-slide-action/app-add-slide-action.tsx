import {Component, Event, EventEmitter, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {BusyService} from '../../../services/editor/busy/busy.service';

@Component({
    tag: 'app-add-slide-action',
    styleUrl: 'app-add-slide-action.scss',
    shadow: false
})
export class AppAddSlideAction {

    @Event() private actionOpenSlideAdd: EventEmitter<UIEvent>;

    private subscription: Subscription;
    private busyService: BusyService;

    @State()
    private deckBusy: boolean = false;

    constructor() {
        this.busyService = BusyService.getInstance();
    }

    private openSlideAdd($event: UIEvent) {
        this.actionOpenSlideAdd.emit($event);
    }

    async componentWillLoad() {
        this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
            this.deckBusy = busy;
        });
    }

    async componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    render() {
        return <ion-tab-button onClick={(e: UIEvent) => this.openSlideAdd(e)} color="primary" disabled={this.deckBusy} mode="md">
            <ion-icon name="add"></ion-icon>
            <ion-label>Add slide</ion-label>
        </ion-tab-button>;
    }

}
