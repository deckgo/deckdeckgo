import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent, Listen} from '@stencil/core';

import {DeckDeckGoRevealComponent} from '@deckdeckgo/slide-utils';

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
export class DeckdeckgoRevealList implements DeckDeckGoRevealComponent {
  @Element() el: HTMLElement;

  @Prop({mutable: true})
  revealProgress: 'start' | 'partial' | 'end' = 'start';

  @Prop()
  listTag: string = 'ol';

  @State()
  private visibleIndex: number = 0;

  @State()
  private focused: boolean = false;

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
          'deckgo-reveal-visible': this.visibleIndex > 0,
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
