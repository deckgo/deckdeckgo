import {Component, Event, EventEmitter, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {DeckBusyService} from '../../../services/deck/deck-busy.service';

@Component({
    tag: 'app-add-slide-action',
    styleUrl: 'app-add-slide-action.scss',
    shadow: false
})
export class AppAddSlideAction {

    @Event() private actionOpenSlideAdd: EventEmitter<UIEvent>;

    private subscription: Subscription;
    private deckBusyService: DeckBusyService;

    @State()
    private deckBusy: boolean = false;

    constructor() {
        this.deckBusyService = DeckBusyService.getInstance();
    }

    private openSlideAdd($event: UIEvent) {
        this.actionOpenSlideAdd.emit($event);
    }

    async componentWillLoad() {
        this.subscription = this.deckBusyService.watch().subscribe((busy: boolean) => {
            this.deckBusy = busy;
        });
    }

    async componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    render() {
        return <ion-button onClick={(e: UIEvent) => this.openSlideAdd(e)} color="primary" shape="round" disabled={this.deckBusy}
                           size="small" fill="solid">
            <ion-label>Add slide</ion-label>
        </ion-button>;
    }

}
