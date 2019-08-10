import { Component, h, Element, Prop, Method, State, Host, FunctionalComponent } from '@stencil/core';

const arrayOf = (len: number) => new Array(len).fill(0).map((_, i) => i);

const RevealNthChild: FunctionalComponent<{ index: number }> = ({ index }) => {
  if (index === 0) return null;
  return (
    <style class={`deckgo-reveal-${index}`}>{`
      ::slotted(*:nth-child(-n+${index})) {
        visibility: initial;
        opacity: 1;
        transform: none;
      };
    `}</style>
  );
}

@Component({
  tag: 'deckgo-reveal',
  styleUrl: 'deckdeckgo-reveal.scss',
  shadow: true
})
export class DeckdeckgoReveal {

  @Element() el: HTMLElement;

  @Prop()
  visible: boolean = false;

  @State()
  private visibleIndex: number = 0;

  @Method()
  display(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.visibleIndex++;

      const elements: Node[] = Array.from(this.el.childNodes).filter((node: Node) => {
        return node && node.nodeType !== node.TEXT_NODE;
      });

      this.visible = elements && elements.length <= this.visibleIndex;

      resolve();
    });
  }

  @Method()
  hide(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.visibleIndex--;

      this.visible = this.visibleIndex !== 0;

      resolve();
    });
  }

  render() {
    return <Host>
      {arrayOf(this.visibleIndex + 1).map(index => <RevealNthChild index={index} />)}
      <slot/>
    </Host>
  }

}
