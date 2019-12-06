import {Component, Element, Event, EventEmitter, h, State} from '@stencil/core';

import {isIPad} from '@deckdeckgo/utils';

import {TargetElement} from '../../../utils/editor/target-element';

@Component({
    tag: 'app-deck-options',
    styleUrl: 'app-deck-options.scss'
})
export class AppDeck {

    @Element() el: HTMLElement;

    @Event() private deckOptionsDidChange: EventEmitter<HTMLElement>;

    @State()
    private applyToTargetElement: TargetElement = TargetElement.DECK;

    @State()
    private moreColors: boolean = true;

    @State()
    private deckElement: HTMLElement;

    async componentWillLoad() {
        this.moreColors = !isIPad();

        this.deckElement = document ? document.querySelector('deckgo-deck') : undefined;
    }

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    private async selectApplyToTargetElement($event: CustomEvent<TargetElement>) {
        if ($event && $event.detail) {
            this.applyToTargetElement = $event.detail;
        }
    }

    private deckOptionsChange() {
        this.deckOptionsDidChange.emit(this.deckElement);
    }

    render() {
        return [<ion-toolbar>
            <h2>Color</h2>
            <ion-router-link slot="end" onClick={() => this.closePopover()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <app-select-target-element deckOrSlide={true}
                                       onApplyTo={($event: CustomEvent<TargetElement>) => this.selectApplyToTargetElement($event)}></app-select-target-element>,

            this.renderOptions()
        ]
    }

    private renderOptions() {
        if (this.applyToTargetElement === TargetElement.DECK) {
            return <app-color-deck-slide selectedElement={this.deckElement} moreColors={this.moreColors}
                                         deckOrSlide={true} applyToAllDeck={true}
                                         onColorChange={() => this.deckOptionsChange()}></app-color-deck-slide>
        } else {
            return undefined;
        }
    }
}
