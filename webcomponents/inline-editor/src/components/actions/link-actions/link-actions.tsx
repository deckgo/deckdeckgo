import {Component, Event, EventEmitter, h, Prop, Host} from '@stencil/core';

import {getSelection} from '@deckdeckgo/utils';

import {AnchorLink, InputTargetEvent} from '../../../interfaces/interfaces';

import {ToolbarActions} from '../../../types/enums';

import {DeckdeckgoInlineEditorUtils} from '../../../utils/utils';

@Component({
  tag: 'deckgo-ie-link-actions',
  styleUrl: 'link-actions.scss',
  shadow: true
})
export class LinkActions {
  private linkUrl: string;

  @Prop()
  toolbarActions: ToolbarActions;

  @Prop()
  anchorLink: AnchorLink;

  @Prop()
  linkCreated: EventEmitter<HTMLElement>;

  @Prop()
  mobile: boolean;

  @Event()
  linkModified: EventEmitter<boolean>;

  @Prop()
  containers: string;

  private handleLinkInput($event: UIEvent) {
    this.linkUrl = ($event.target as InputTargetEvent).value;
  }

  private createLink() {
    if (!document) {
      return;
    }

    if (!this.anchorLink) {
      return;
    }

    if (!this.linkUrl || this.linkUrl.length <= 0) {
      return;
    }

    const selection: Selection | null = getSelection();
    let targetContainer: Node = this.anchorLink.range.commonAncestorContainer
      ? this.anchorLink.range.commonAncestorContainer
      : selection?.anchorNode;

    if (!targetContainer) {
      return;
    }

    // If node text
    if (targetContainer.nodeType === Node.TEXT_NODE || targetContainer.nodeType === Node.COMMENT_NODE) {
      targetContainer = targetContainer.parentElement;
    }

    const target: Node = Array.from(targetContainer.childNodes).find((node: Node) => {
      return node.textContent && node.textContent.trim().indexOf(this.anchorLink.text) > -1;
    });

    if (!target) {
      return;
    }

    if (target.nodeType === 3) {
      const index: number = target.textContent.indexOf(this.anchorLink.text);

      const textBefore: string = index > -1 ? target.textContent.substr(0, index) : null;
      const textAfter: string =
        index + this.anchorLink.text.length > -1 ? target.textContent.substr(index + this.anchorLink.text.length) : null;

      if (textBefore) {
        target.parentElement.insertBefore(document.createTextNode(textBefore), target);
      }

      const a: HTMLAnchorElement = this.createLinkElement();
      target.parentElement.insertBefore(a, target);

      if (textAfter) {
        target.parentElement.insertBefore(document.createTextNode(textAfter), target);
      }

      target.parentElement.removeChild(target);
    } else {
      const a: HTMLAnchorElement = this.createLinkElement();

      target.parentElement.replaceChild(a, target);
    }

    const container: HTMLElement | undefined = DeckdeckgoInlineEditorUtils.findContainer(this.containers, targetContainer as HTMLElement);

    if (!container) {
      return;
    }

    this.linkCreated.emit(container);
  }

  private createLinkElement(): HTMLAnchorElement {
    const a: HTMLAnchorElement = document.createElement('a');
    const linkText: Text = document.createTextNode(this.anchorLink.text);

    a.appendChild(linkText);
    a.title = this.anchorLink.text;
    a.href = this.linkUrl;

    return a;
  }

  private handleLinkEnter($event: KeyboardEvent) {
    if (!$event) {
      return;
    }

    if (
      this.toolbarActions === ToolbarActions.SELECTION &&
      ($event.key.toLowerCase() === 'backspace' || $event.key.toLowerCase() === 'delete')
    ) {
      this.linkModified.emit(false);
    } else if (this.toolbarActions === ToolbarActions.LINK && $event.key.toLowerCase() === 'enter') {
      this.createLink();
      this.linkModified.emit(true);
    }
  }

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <input
          autofocus
          placeholder="Add a link..."
          onInput={($event: UIEvent) => this.handleLinkInput($event)}
          onKeyUp={($event: KeyboardEvent) => this.handleLinkEnter($event)}></input>
      </Host>
    );
  }
}
