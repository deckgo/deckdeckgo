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

  private slotTypes: SlotType[] = [SlotType.H1, SlotType.H2, SlotType.H3, SlotType.CODE, SlotType.IMG, SlotType.OL];

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
    focusParagraph({paragraph: this.paragraph});

    if ([SlotType.H1, SlotType.H2, SlotType.H3].includes(slotType)) {
      this.transformSlotTitle(slotType);
    } else if ([SlotType.OL, SlotType.UL].includes(slotType)) {
      this.addList();
    } else {
      this.addElementInSection(slotType);
    }

    this.hide();
  }

  private transformSlotTitle(slotType: SlotType) {
    document.execCommand('formatBlock', false, slotType.toLowerCase());
  }

  private addElementInSection(slotType: SlotType) {
    const element: HTMLElement = createHTMLElement({slotType});

    if (SlotUtils.isNodeEditable(element)) {
      element.setAttribute('editable', 'true');
    }

    document.execCommand('insertHTML', false, `${element.outerHTML}`);
  }

  /**
   * In case of list we do a hack and move the list outside of the section / div in which it is rendered.
   * Doing we unfortunately loose the "redo" option (undo will work).
   * Problem is that subsequent paragraphs are going to be added within the same paragraph that contains the list.
   * Not perfect though would still need improvements.
   * @param slotType
   * @private
   */
  private addList() {
    const onRender = (mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      const newNode: Node | undefined = mutations[0]?.addedNodes?.[0];

      // Move for flattening paragraphs
      this.containerRef.insertBefore(newNode, newNode?.parentNode);
      newNode?.parentNode.removeChild(newNode?.nextSibling);

      focusParagraph({paragraph: newNode as HTMLElement});
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});

    document.execCommand('insertOrderedList', false);
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
        <app-slot-type
          slotTypes={this.slotTypes}
          onSelectType={({detail}: CustomEvent<SlotType>) => this.transformSlot(detail)}></app-slot-type>
      </Host>
    );
  }
}
