import {Component, Event, EventEmitter, h, Host, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {Deck} from '../../../models/data/deck';

import {MoreAction} from '../../../utils/editor/more-action';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-share-options',
    styleUrl: 'app-share-options.scss',
    shadow: true
})
export class AppMoreShareOptions {

    @Event() selectedOption: EventEmitter<MoreAction>;

    @State()
    private published: boolean = false;

    private deckEditorService: DeckEditorService;
    private subscription: Subscription;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.deckEditorService.watch().subscribe(async (deck: Deck) => {
            this.published = deck && deck.data && deck.data.meta && deck.data.meta.published;
        });
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    render() {
        return <Host>
            {this.renderUpdate()}
            <a onClick={() => this.selectedOption.emit(this.published ? MoreAction.SHARE : MoreAction.PUBLISH)}><p>Share</p></a>
        </Host>
    }

    private renderUpdate() {
        if (this.published) {
            return <a onClick={() => this.selectedOption.emit(MoreAction.PUBLISH)}><p>Update your published presentation</p></a>;
        } else {
            return undefined;
        }
    }

}
