import {Component, h, Listen, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {Remarkable} from 'remarkable';

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
    private notes: string;

    constructor() {
        this.notesService = NotesService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.notesService.watch().subscribe((slide: DeckdeckgoSlideDefinition) => {
            let notes: string = undefined;

            if (document && slide && slide.content) {
                const element: HTMLDivElement = document.createElement('div');
                element.innerHTML = slide.content;

                const notesElement: HTMLElement = element.querySelector('[slot=\'notes\']');

                if (notesElement && notesElement.innerHTML !== '') {
                    const md: Remarkable = new Remarkable({
                        html: true,
                        xhtmlOut: true,
                        breaks: true
                    });

                    const codeRule = (inline: boolean) => (tokens, idx, _options, _env) => {

                        console.log(tokens[idx]);

                        return `<deckgo-highlight-code 
                                ${inline ? 'class="inline"' : ''}
                                language="${tokens[idx].params ? tokens[idx].params : 'javascript'}">
                                    <code slot="code">${tokens[idx].content}</code>
                            </deckgo-highlight-code>`;
                    };

                    md.renderer.rules.code = codeRule(true);
                    md.renderer.rules.fence = codeRule(false);

                    notes = md.render(notesElement.innerHTML.replace(/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/gmi, ''));
                }
            }

            this.notes = notes;
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
            <p slot="sheet-header" class="ion-margin-start ion-margin-end ion-text-uppercase">Notes</p>,
            this.renderNote()
        ]
    }

    private renderNote() {
        if (!this.notes || this.notes === undefined) {
            return undefined;
        }

        return <div class="ion-padding-top ion-padding-bottom ion-margin notes" innerHTML={this.notes}></div>;
    }
}
