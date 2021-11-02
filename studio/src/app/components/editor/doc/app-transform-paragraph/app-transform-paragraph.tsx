import {Component, ComponentInterface, h, Host, Listen, Prop, State} from '@stencil/core';

import {SlotType} from '../../../../types/editor/slot-type';

import {createHTMLElement} from '../../../../utils/editor/create-element.utils';
import {SlotUtils} from '../../../../utils/editor/slot.utils';
import {focusParagraph} from '../../../../utils/editor/paragraph.utils';

@Component({
  tag: 'app-transform-paragraph',
  styleUrl: 'app-transform-paragraph.scss',
  shadow: false
})
export class AppTransformParagraph implements ComponentInterface {
  @Prop()
  containerRef: HTMLElement | undefined;

  @State()
  private display: boolean = false;

  @State()
  private position: {left: number; top: number; downward: boolean} | undefined = undefined;

  private paragraph: HTMLElement | undefined | null;

  componentDidRender() {
    this.display = this.position !== undefined;
  }

  @Listen('keydown', {target: 'document', passive: true})
  onKeyDown(_$event: KeyboardEvent) {
    this.hide();
  }

  @Listen('click', {target: 'document', passive: true})
  onMouseDown(_$event: MouseEvent | TouchEvent) {
    this.hide();
  }

  @Listen('sizeDidChange', {target: 'document', passive: true})
  onSizeDidChange(_$event: CustomEvent<{width: number; height: number}>) {
    this.hide();
  }

  private hide() {
    this.position = undefined;
    this.paragraph = undefined;
  }

  @Listen('selectParagraph', {target: 'document', passive: true})
  onSelectParagraph({detail: element}: CustomEvent<HTMLElement | undefined>) {
    if (!element) {
      this.hide();
      return;
    }

    const {left, height, top}: DOMRect = element.getBoundingClientRect();

    // top + size + margin
    const downward: boolean = top + 220 + 16 < (window.innerHeight || screen.height);

    this.position = {
      top: element.offsetTop + (downward ? height : -1 * height),
      downward,
      left: left
    };

    this.paragraph = element;
  }

  private transformSlot(slotType: SlotType | null) {
    const element: HTMLElement = createHTMLElement({slotType});

    if (SlotUtils.isNodeEditable(element)) {
      element.setAttribute('editable', 'true');
    }

    focusParagraph({paragraph: this.paragraph});

    const onRender = (mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      const newNode: Node | undefined = mutations[0]?.addedNodes?.[0];

      console.log('newNode', newNode);

      // Move for flattening paragraphs
      // this.containerRef.insertBefore(newNode, newNode?.parentNode);
      // newNode?.parentNode.removeChild(newNode?.nextSibling);

      // TODO: Delete not OK and force save
      // List in inline-editor?

      // focusParagraph({paragraph: newNode as HTMLElement});
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});

    document.execCommand('insertHTML', false, `${element.outerHTML}`);

    // document.execCommand('insertParagraph', false);

    // document.execCommand('formatBlock', false, 'h3');

    // document.execCommand('insertOrderedList', false);

    this.hide();
  }

  render() {
    const style: Record<string, string> =
      this.position === undefined
        ? {}
        : {
            '--actions-top': `${this.position.top}px`,
            '--actions-left': `${this.position.left}px`,
            '--actions-translate-y': `${this.position.downward ? '0' : '-100%'}`
          };

    return (
      <Host style={style} class={this.display ? 'display' : 'hidden'}>
        <app-slot-type onSelectType={({detail}: CustomEvent<SlotType>) => this.transformSlot(detail)}></app-slot-type>
      </Host>
    );
  }
}
