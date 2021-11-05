import {Component, ComponentInterface, Host, h, Element, State, Prop, Watch, EventEmitter, Event} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import {containerSize} from '../utils/size.utils';

@Component({
  tag: 'deckgo-doc',
  styleUrl: 'doc.scss',
  shadow: true
})
export class DeckGoDoc implements ComponentInterface {
  @Element() el: HTMLElement;

  @Prop({reflect: true})
  size: 'A4' | 'A3' | 'A5' = 'A4';

  @Prop({reflect: true})
  orientation: 'portrait' | 'landscape' = 'portrait';

  @Prop()
  zoom: number;

  @State()
  private docSize: {width: number; height: number};

  @Event()
  sizeDidChange: EventEmitter<{width: number; height: number}>;

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
    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();
      this.sizeDidChange.emit(this.docSize);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.el, {attributes: true});

    this.docSize = containerSize({embedded: true, container: this.el});
  }

  private initWindowResize() {
    window?.addEventListener('resize', this.debounceInitSize);
  }

  render() {
    const style: Record<string, string> = {
      ...(this.zoom && {'--doc-zoom': `${this.zoom}`}),
      ...(this.docSize && {'--doc-width': `${this.docSize.width}px`, '--doc-height': `${this.docSize.height}px`})
    };

    return (
      <Host style={style}>
        <slot></slot>
      </Host>
    );
  }
}
