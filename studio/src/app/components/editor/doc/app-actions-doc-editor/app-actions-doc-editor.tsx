import {Component, Listen, h, State, Host, Prop} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {findParagraph} from '../../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../../utils/editor/node.utils';

import {AppIcon} from '../../../core/app-icon/app-icon';

@Component({
  tag: 'app-actions-doc-editor',
  styleUrl: 'app-actions-doc-editor.scss',
  shadow: false
})
export class AppActionsDocEditor {
  @Prop()
  containerRef: HTMLElement | undefined;

  @State()
  private top: number | undefined;

  @State()
  private left: number | undefined;

  @Listen('keydown', {target: 'document', passive: true})
  onKeyDown({code}: KeyboardEvent) {
    if (code !== 'Enter') {
      if (this.top !== undefined) {
        this.hide();
      }

      return;
    }

    const onRender = (mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();
      this.initPosition(mutations[0]?.addedNodes?.[0]);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  @Listen('mouseup', {target: 'document', passive: true})
  onMouseDown({target}: MouseEvent) {
    this.initPosition(target);
  }

  @Listen('touchend', {target: 'document', passive: true})
  onTouchStart({target}: TouchEvent) {
    this.initPosition(target);
  }

  @Listen('sizeDidChange', {target: 'document', passive: true})
  onSizeDidChange({target}: CustomEvent<{width: number; height: number}>) {
    console.log(target, (target as HTMLElement)?.getBoundingClientRect()?.left);

    this.left = (target as HTMLElement)?.getBoundingClientRect()?.left;
  }

  private hide() {
    this.top = undefined;
  }

  private initPosition(target: EventTarget | null) {
    if (!this.containerRef || !target) {
      this.hide();
      return;
    }

    const paragraph: Node | undefined = findParagraph({element: target as Node, container: this.containerRef});

    const element: HTMLElement | undefined | null = NodeUtils.toHTMLElement(paragraph);

    if (!element) {
      this.hide();
      return;
    }

    this.top = element.offsetTop;
  }

  private action() {
    // TODO: color bug and other small issues

    document.execCommand('insertHTML', false, '<span>HERE</span>');
  }

  render() {
    const style: Record<string, string> =
      this.top === undefined || this.left === undefined
        ? {display: 'none'}
        : {'--actions-top': `${this.top}px`, '--actions-left': `${this.left}px`};

    return (
      <Host style={style}>
        <button
          aria-label={i18n.state.editor.add_element}
          onClick={() => this.action()}
          class="ion-activatable"
          onMouseDown={($event) => $event.stopPropagation()}
          onTouchStart={($event) => $event.stopPropagation()}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="add" ariaLabel="" ariaHidden={true}></AppIcon>
          <slot></slot>
        </button>
      </Host>
    );
  }
}
