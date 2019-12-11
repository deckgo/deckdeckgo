import {h} from '@stencil/core';

import {ParseElementsUtils} from './parse-elements.utils';

export class ParseBackgroundUtils {

    static convertBackground(background: string, contentEditable: boolean): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!background || background === undefined || background === '') {
                resolve(undefined);
                return;
            }

            const div = document.createElement('div');
            div.setAttribute('slot', 'background');
            div.innerHTML = background;

            const content = await ParseElementsUtils.parseElements(div, true, contentEditable);

            resolve(<div slot="background">
                {content}
            </div>);
        });
    }

    static stickDeckBackgroundLastChild(el: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!el) {
                resolve();
                return;
            }

            const deck: HTMLElement = el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            const background: HTMLElement = el.querySelector('deckgo-deck > [slot=\'background\']');

            if (!background) {
                resolve();
                return;
            }

            // Wait for next new slide to be loaded
            deck.addEventListener('slideDidLoad', async () => {
                // Append the child to move it to the last position (if exists, append act as a move to last child)
                deck.appendChild(background);
            }, {once: true});

            resolve();
        });
    }
}
