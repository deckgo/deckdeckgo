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
}
