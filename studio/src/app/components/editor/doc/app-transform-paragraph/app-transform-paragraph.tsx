import {Component, ComponentInterface, h, Host, Listen, State} from '@stencil/core';

@Component({
  tag: 'app-transform-paragraph',
  styleUrl: 'app-transform-paragraph.scss',
  shadow: false
})
export class AppTransformParagraph implements ComponentInterface {
  @State()
  private display: boolean = false;

  @State()
  private position: {left: number; top: number} | undefined = undefined;

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
  }

  @Listen('selectParagraph', {target: 'document', passive: true})
  onSelectParagraph({detail: element}: CustomEvent<HTMLElement | undefined>) {
    if (!element) {
      this.hide();
      return;
    }

    const {left, height}: DOMRect = element.getBoundingClientRect();

    this.position = {
      top: element.offsetTop + height,
      left: left
    };
  }

  render() {
    const style: Record<string, string> =
      this.position === undefined ? {} : {'--actions-top': `${this.position.top}px`, '--actions-left': `${this.position.left}px`};

    return (
      <Host style={style} class={this.display ? 'display' : 'hidden'}>
        <app-slot-type></app-slot-type>
      </Host>
    );
  }
}
