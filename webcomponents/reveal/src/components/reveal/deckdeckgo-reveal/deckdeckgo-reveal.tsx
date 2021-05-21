import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent} from '@stencil/core';

import { DeckDeckGoRevealComponent } from "@deckdeckgo/slide-utils";

import {DeckdeckgoRevealUtils} from '../deckdeckgo-reveal-utils';

const RevealNthChild: FunctionalComponent<{index: number}> = ({index}) => {
  if (index === 0) {
    return undefined;
  }

  // Note: "transition: none;" is needed otherwise, the content won't be editable if set as such!

  return (
    <style class={`deckgo-reveal-${index}`}>{`
      ::slotted(*:nth-child(-n+${index})) {
        visibility: initial;
        opacity: 1;
        transform: none;
        transition: none;
      };
    `}</style>
  );
};

@Component({
  tag: 'deckgo-reveal',
  styleUrl: 'deckdeckgo-reveal.scss',
  shadow: true,
})
export class DeckdeckgoReveal implements DeckDeckGoRevealComponent {
  @Element() el: HTMLElement;

  @Prop({mutable: true})
  revealProgress: 'start' | 'partial' | 'end' = 'start';

  @State()
  private visibleIndex: number = 0;

  @Method()
  async reveal() {
    this.visibleIndex++;

    const elements: Node[] = await DeckdeckgoRevealUtils.findChildren(this.el);

    this.revealProgress = elements && elements.length <= this.visibleIndex ? 'end' : 'partial';
  }

  @Method()
  async hide() {
    this.visibleIndex--;

    this.revealProgress = this.visibleIndex === 0 ? 'start' : 'partial';
  }

  @Method()
  async revealAll() {
    const elements: Node[] = await DeckdeckgoRevealUtils.findChildren(this.el);

    this.visibleIndex = elements ? elements.length : 0;

    this.revealProgress = 'end';
  }

  @Method()
  async hideAll() {
    this.visibleIndex = 0;

    this.revealProgress = 'start';
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-reveal-visible': this.visibleIndex > 0,
        }}>
        {<RevealNthChild index={this.visibleIndex} />}
        <slot />
      </Host>
    );
  }
}
