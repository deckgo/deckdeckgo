import {Component, Listen, h, State, Host, Prop} from '@stencil/core';

import {moveCursorToEnd} from '@deckdeckgo/utils';

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

  private paragraph: HTMLElement | undefined | null;

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

  @Listen('click', {target: 'document', passive: true})
  onMouseDown({target}: MouseEvent | TouchEvent) {
    this.initPosition(target);
  }

  @Listen('sizeDidChange', {target: 'document', passive: true})
  onSizeDidChange({target}: CustomEvent<{width: number; height: number}>) {
    this.left = (target as HTMLElement)?.getBoundingClientRect()?.left;
  }

  private hide() {
    this.top = undefined;
    this.paragraph = undefined;
  }

  private initPosition(target: EventTarget | null) {
    if (!this.containerRef || !target) {
      this.hide();
      return;
    }

    const paragraph: Node | undefined = findParagraph({element: target as Node, container: this.containerRef});

    this.paragraph = NodeUtils.toHTMLElement(paragraph);

    if (!this.paragraph) {
      this.hide();
      return;
    }

    this.top = this.paragraph.offsetTop;
  }

  // TODO: color bug and other small issues
  // TODO: bug remove title text not saved
  // TODO: bug "new doc -> new doc -> reload -> title twice"

  private initParagraph($event: UIEvent) {
    if (!this.paragraph) {
      return;
    }

    $event.stopPropagation();

    if (this.paragraph.textContent === '') {
      // TODO: display to create paragraph list

      this.focusParagraph();

      return;
    }

    this.focusParagraph();

    // \n is useful to create a new adjacent node and not a node within the current paragraph
    document.execCommand('insertHTML', false, '\n<section>\n</section>');

    // TODO: display to create paragraph list

    this.hide();
  }

  private focusParagraph() {
    this.paragraph.focus();
    moveCursorToEnd(this.paragraph);
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
          onClick={($event: UIEvent) => this.initParagraph($event)}
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
