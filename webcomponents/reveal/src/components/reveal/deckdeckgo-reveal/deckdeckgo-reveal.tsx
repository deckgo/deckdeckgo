import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent} from '@stencil/core';

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
export class DeckdeckgoReveal {
  @Element() el: HTMLElement;

  @Prop()
  allElementsRevealed: boolean = false;

  @Prop()
  allElementsHidden: boolean = true;

  @State()
  private visibleIndex: number = 0;

  @Method()
  reveal(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.visibleIndex++;

      const elements: Node[] = await DeckdeckgoRevealUtils.findChildren(this.el);

      this.allElementsRevealed = elements && elements.length <= this.visibleIndex;
      this.allElementsHidden = false;

      resolve();
    });
  }

  @Method()
  hide(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.visibleIndex--;

      this.allElementsHidden = this.visibleIndex === 0;
      this.allElementsRevealed = false;

      resolve();
    });
  }

  @Method()
  revealAll(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const elements: Node[] = await DeckdeckgoRevealUtils.findChildren(this.el);

      this.visibleIndex = elements ? elements.length : 0;

      this.allElementsRevealed = true;
      this.allElementsHidden = false;

      resolve();
    });
  }

  @Method()
  hideAll(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.visibleIndex = 0;

      this.allElementsHidden = true;
      this.allElementsRevealed = false;

      resolve();
    });
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
