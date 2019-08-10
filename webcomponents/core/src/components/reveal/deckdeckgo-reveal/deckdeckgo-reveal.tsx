import {Component, h, Element, Prop, Method, State, Host} from '@stencil/core';

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
        return node && node.nodeType !== 3;
      });

      this.visible = elements && elements.length <= this.visibleIndex;

      const style: HTMLStyleElement = document.createElement('style');
      style.className = `display-${this.visibleIndex}`;
      style.innerHTML = `::slotted(*:nth-child(-n+${this.visibleIndex})) {
        visibility: initial;
        opacity: 1;
        transform: none;
      }`;
      this.el.shadowRoot.appendChild(style);

      resolve();
    });
  }

  @Method()
  hide(): Promise<void> {
    return new Promise<void>((resolve) => {
      const target: Node = Array.from(this.el.shadowRoot.childNodes).find((node: Node) => {
        return node instanceof HTMLStyleElement && node.className === `display-${this.visibleIndex}`;
      });

      if (target) {
        this.el.shadowRoot.removeChild(target);
      }

      this.visibleIndex--;

      this.visible = this.visibleIndex !== 0;

      resolve();
    });
  }

  render() {
    return <Host>
      <slot/>
    </Host>
  }

}
