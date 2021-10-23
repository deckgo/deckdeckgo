import {Component, ComponentInterface, Host, h, Prop, Element, Event, EventEmitter} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import {getSelection} from '../utils/inline-editor/selection.utils';
import {getAnchorElement} from '../utils/inline-editor/node.utils';

import {findParagraph} from '../utils/container.utils';

@Component({
  tag: 'deckgo-page',
  styleUrl: 'page.scss',
  shadow: true
})
export class DeckGoPage implements ComponentInterface {
  @Element() el: HTMLElement;

  @Prop({reflect: true})
  size: 'A4' | 'A3' | 'A5' = 'A4';

  @Prop({reflect: true})
  orientation: 'portrait' | 'landscape' = 'portrait';

  @Event()
  pageEnd: EventEmitter<Node>;

  private readonly debouncePageEnd: () => void = debounce(() => this.detectPageEnd(), 100);

  private observer: MutationObserver | undefined;

  private slotRef: Node | null;

  componentDidLoad() {
    this.observer = new MutationObserver(this.onMutation);
    this.observer.observe(this.el, {childList: true, subtree: true, characterData: true});
  }

  disconnectedCallback() {
    this.observer?.disconnect();
  }

  private onMutation = (_mutations: MutationRecord[]) => {
    // console.log(
    //   mutations,
    //   this.el.scrollHeight,
    //   this.el.offsetHeight, // --> value container height
    //   (mutations[0].target as HTMLElement).offsetHeight,
    //   (mutations[0].target as HTMLElement).offsetTop,
    //   mutations[1].target as HTMLElement,
    //   (mutations[1].target as HTMLElement).offsetHeight, // --> element height
    //   (mutations[1].target as HTMLElement).offsetTop,
    //   (mutations[1].target as HTMLElement).getClientRects()[0].top, // --> element position
    //   mutations[0].target.parentElement.getClientRects().length // display: inline
    // );

    this.debouncePageEnd();
  };

  private detectPageEnd = () => {
    const selection: Selection | undefined = getSelection();
    const element: HTMLElement | undefined = getAnchorElement(selection);

    const paragraph: HTMLElement | undefined = findParagraph({element, container: this.slotRef});

    if (!paragraph) {
      return;
    }

    const top: number = paragraph.offsetTop;
    const offset: number = paragraph.offsetHeight;

    // We also check top > offset to not throw the error on the first line
    if (this.el.scrollHeight < top + offset && top > offset) {
      this.pageEnd.emit(paragraph);
    }
  };

  private setSlotRef({target}: SlotChangeEvent) {
    this.slotRef = target?.assignedElements()?.[0];
  }

  render() {
    return (
      <Host>
        <slot onSlotchange={($event: SlotChangeEvent) => this.setSlotRef($event)}></slot>
      </Host>
    );
  }
}
