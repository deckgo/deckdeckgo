import {Component, h, Element, Prop, Method, State, Host, FunctionalComponent} from '@stencil/core';

const RevealNthChild: FunctionalComponent<{ index: number }> = ({index}) => {
  if (index === 0) {
    return undefined;
  }

  return (
    <style class={`deckgo-reveal-${index}`}>{`
      ::slotted(*:nth-child(-n+${index})) {
        visibility: initial;
        opacity: 1;
        transform: none;
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
  allElementsHidden: boolean = false;

  @State()
  private visibleIndex: number = 0;

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

  render() {
    return <Host>
      {<RevealNthChild index={this.visibleIndex}/>}
      <slot/>
    </Host>
  }

}
