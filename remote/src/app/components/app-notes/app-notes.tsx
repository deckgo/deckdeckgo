import {Component, h, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {DeckdeckgoSlideDefinition} from '@deckdeckgo/types';

import {NotesService} from '../../services/notes/notes.service';

@Component({
    tag: 'app-notes',
    styleUrl: 'app-notes.scss',
})
export class AppNotes {

    private notesService: NotesService;

    private subscription: Subscription;

    @State()
    private currentSlide: DeckdeckgoSlideDefinition;

    constructor() {
        this.notesService = NotesService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.notesService.watch().subscribe((slide:  DeckdeckgoSlideDefinition) => {
            this.currentSlide = slide;
        })
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    render() {
        if (!this.currentSlide || ! this.currentSlide.notes || this.currentSlide.notes === '') {
            return undefined;
        }

        return <p class="ion-padding notes">{this.currentSlide.notes.replace(/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/gmi, '')}</p>;
    }
}
