import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent, Listen} from '@stencil/core';

const RevealNthChild: FunctionalComponent<{ index: number }> = ({index}) => {
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
  tag: 'deckgo-reveal',
  styleUrl: 'deckdeckgo-reveal.scss',
  shadow: true
})
export class DeckdeckgoReveal {

  @Element() el: HTMLElement;

  @Prop()
  allElementsRevealed: boolean = false;

  @Prop()
  allElementsHidden: boolean = true;

  @Prop()
  list: string;

  @State()
  private visibleIndex: number = 0;

  @State()
  private focused: boolean = false;

  @Method()
  reveal(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.visibleIndex++;

      const elements: Node[] = await this.findChildren();

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
      const elements: Node[] = await this.findChildren();

      this.visibleIndex =  elements ? elements.length : 0;

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

  private findChildren(): Promise<Node[]> {
    return new Promise<Node[]>((resolve) => {
      const elements: Node[] = Array.from(this.el.childNodes).filter((node: Node) => {
        return node && node.nodeType !== node.TEXT_NODE;
      });

      resolve(elements);
    })
  }

  @Listen('focus')
  onFocus() {
    this.focused = true;
  }

  @Listen('blur')
  onBlur() {
    this.focused = false;
  }

  render() {
    return <Host class={{
        'deckgo-reveal-all': this.focused
      }}>
      {<RevealNthChild index={this.visibleIndex}/>}
      {this.renderContent()}
    </Host>
  }

  private renderContent() {
    if (this.list) {
      return this.renderList();
    } else {
      return <slot/>;
    }
  }

  private renderList() {
    const Element: string = this.list;

    return <Element>
      <slot/>
    </Element>
  }

}
