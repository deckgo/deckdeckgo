import {Component, Listen, h, State, Host, Prop, ComponentInterface, EventEmitter, Event} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {findParagraph, focusParagraph} from '../../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../../utils/editor/node.utils';

import {AppIcon} from '../../../core/app-icon/app-icon';

@Component({
  tag: 'app-add-paragraph',
  styleUrl: 'app-add-paragraph.scss',
  shadow: false
})
export class AppAddParagraph implements ComponentInterface {
  @Prop()
  containerRef: HTMLElement | undefined;

  @State()
  private top: number | undefined;

  @State()
  private left: number | undefined;

  private paragraph: HTMLElement | undefined | null;

  @Event()
  selectParagraph: EventEmitter<HTMLElement | undefined>;

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
  }

  private initPosition = (target: EventTarget | null) => {
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
  };

  private initParagraph($event: UIEvent) {
    if (!this.paragraph) {
      return;
    }

    $event.stopPropagation();

    if (['', '\n'].includes(this.paragraph.textContent)) {
      focusParagraph({paragraph: this.paragraph});

      this.selectParagraph.emit(this.paragraph);

      return;
    }

    focusParagraph({paragraph: this.paragraph});

    const onRender = (mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      const addedNodes: Node[] = mutations.reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);
      const section: Node | undefined = addedNodes.find((node: Node) => node.nodeName.toLowerCase() === 'section');

      this.selectParagraph.emit(section as HTMLElement | undefined);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});

    // \n is useful to create a new adjacent node and not a node within the current paragraph
    document.execCommand('insertHTML', false, '\n<section>\n</section>');

    this.hide();
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
