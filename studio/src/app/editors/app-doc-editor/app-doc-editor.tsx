import {Component, ComponentInterface, Fragment, h} from '@stencil/core';

import colorStore from '../../stores/color.store';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  render() {
    return (
      <Fragment>
        <ion-content class={`ion-no-padding`}>
          <main>
            <deckgo-doc>
              <article contentEditable={true}>Hello World</article>
            </deckgo-doc>
          </main>
        </ion-content>

        <deckgo-inline-editor
          containers="article"
          sticky-mobile="true"
          img-anchor="deckgo-lazy-img"
          list={true}
          palette={colorStore.state.history}
          align={true}
          fontSize={true}></deckgo-inline-editor>
      </Fragment>
    );
  }
}
