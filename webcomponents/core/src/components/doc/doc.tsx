import {Component, ComponentInterface, Host, h, Element, State, Prop, Watch} from '@stencil/core';
import {containerSize} from '../utils/size.utils';
import {debounce} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-doc',
  styleUrl: 'doc.scss',
  shadow: true
})
export class DeckGoDoc implements ComponentInterface {
  @Element() el: HTMLElement;

  @Prop()
  zoom: number;

  @State()
  private size: {width: number; height: number};

  private readonly debounceInitSize: () => void = debounce(() => this.initSize(), 100);

  componentDidLoad() {
    this.initSize();
    this.initWindowResize();
  }

  disconnectedCallback() {
    window?.removeEventListener('resize', this.debounceInitSize);
  }

  @Watch('zoom')
  onZoomChange() {
    this.debounceInitSize();
  }

  private initSize() {
    this.size = containerSize({embedded: true, container: this.el});
  }

  private initWindowResize() {
    window?.addEventListener('resize', this.debounceInitSize);
  }

  render() {
    const style: Record<string, string> = {
      ...(this.zoom && {'--doc-zoom': `${this.zoom}`}),
      ...(this.size && {'--doc-width': `${this.size.width}px`, '--doc-height': `${this.size.height}px`})
    };

    return (
      <Host style={style}>
        <slot></slot>
      </Host>
    );
  }
}
