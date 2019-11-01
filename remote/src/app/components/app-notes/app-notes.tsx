import {Component, h, Listen, State} from '@stencil/core';

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
    private portrait: boolean = true;

    @State()
    private currentSlide: DeckdeckgoSlideDefinition;

    constructor() {
        this.notesService = NotesService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.notesService.watch().subscribe((slide: DeckdeckgoSlideDefinition) => {
            this.currentSlide = slide;
        });
    }

    componentDidLoad() {
        this.initPortrait();
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    @Listen('resize', {target: 'window'})
    onOrientationchange() {
        this.initPortrait();
    }

    private initPortrait() {
        this.portrait = window.matchMedia('(orientation: portrait)').matches;
    }

    render() {
        if (this.portrait) {
            return <bottom-sheet arrow={true}>
                {this.renderNotes()}
            </bottom-sheet>;
        } else {
            return <div class="ion-padding landscape-notes">
                {this.renderNotes()}
            </div>
        }
    }

    private renderNotes() {
        return [
            <p slot="sheet-header">Notes</p>,
            this.renderNote()
        ]
    }

    private renderNote() {
        if (!this.currentSlide || !this.currentSlide.notes || this.currentSlide.notes === '') {
            return undefined;
        }

        return <p class="ion-padding-top ion-padding-bottom ion-margin-top notes">{this.currentSlide.notes.replace(/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/gmi, '')}</p>;
    }
}
