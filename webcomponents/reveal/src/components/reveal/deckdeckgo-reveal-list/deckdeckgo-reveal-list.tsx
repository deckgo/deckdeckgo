import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent, Listen} from '@stencil/core';

import {DeckdeckgoRevealUtils} from '../deckdeckgo-reveal-utils';

const RevealListNthChild: FunctionalComponent<{index: number}> = ({index}) => {
  if (index === 0) {
    return undefined;
  }

  // Note: "transition: none;" is needed otherwise, the content won't be editable if set as such!

  return (
    <style class={`deckgo-reveal-${index}`}>{`
      :host(:not(.deckgo-reveal-all)) ::slotted(*:nth-child(-n+${index})) {
        visibility: initial;
        opacity: 1;
        transform: none;
        transition: none;
      };
    `}</style>
  );
};

@Component({
  tag: 'deckgo-reveal-list',
  styleUrl: 'deckdeckgo-reveal-list.scss',
  shadow: true,
})
export class DeckdeckgoRevealList {
  @Element() el: HTMLElement;

  @Prop()
  allElementsRevealed: boolean = false;

  @Prop()
  allElementsHidden: boolean = true;

  @Prop()
  listTag: string = 'ol';

  @State()
  private visibleIndex: number = 0;

  @State()
  private focused: boolean = false;

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

  @Listen('focus')
  onFocus() {
    this.focused = true;
  }

  @Listen('blur')
  async onBlur() {
    this.focused = false;

    await this.revealAll();
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-reveal-all': this.focused,
        }}>
        {<RevealListNthChild index={this.visibleIndex} />}
        {this.renderList()}
      </Host>
    );
  }

  private renderList() {
    const Element: string = this.listTag;

    return (
      <Element>
        <slot />
      </Element>
    );
  }
}
