import {Component, h, Listen, State} from '@stencil/core';

import notesStores from '../../stores/notes.store';

import {Remarkable} from 'remarkable';

@Component({
  tag: 'app-notes',
  styleUrl: 'app-notes.scss',
})
export class AppNotes {
  private destroyListener;

  @State()
  private portrait: boolean = true;

  @State()
  private notes: string;

  componentWillLoad() {
    notesStores.onChange('currentSlide', (element: HTMLElement) => {
      let notes: string = undefined;

      if (document && element) {
        const notesElement: HTMLElement = element.querySelector("[slot='notes']");

        if (notesElement && notesElement.innerHTML !== '') {
          const md: Remarkable = new Remarkable({
            html: true,
            xhtmlOut: true,
            breaks: true,
          });

          const codeRule = (inline: boolean) => (tokens, idx, _options, _env) => {
            return `<deckgo-highlight-code 
                                ${inline ? 'class="inline"' : ''}
                                language="${tokens[idx].params ? tokens[idx].params : 'javascript'}">
                                    <code slot="code">${tokens[idx].content}</code>
                            </deckgo-highlight-code>`;
          };

          md.renderer.rules.code = codeRule(true);
          md.renderer.rules.fence = codeRule(false);

          notes = md.render(notesElement.innerHTML.replace(/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/gim, ''));
        }
      }

      this.notes = notes;
    });
  }

  componentDidLoad() {
    this.initPortrait();
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
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
      return <app-bottom-sheet>{this.renderNotes()}</app-bottom-sheet>;
    } else {
      return <div class="ion-padding landscape-notes">{this.renderNotes()}</div>;
    }
  }

  private renderNotes() {
    return [<p class="ion-margin-start ion-margin-end ion-text-uppercase">Notes</p>, this.renderNote()];
  }

  private renderNote() {
    if (!this.notes || this.notes === undefined) {
      return undefined;
    }

    return <div class="ion-padding-top ion-padding-bottom ion-margin notes" innerHTML={this.notes}></div>;
  }
}
