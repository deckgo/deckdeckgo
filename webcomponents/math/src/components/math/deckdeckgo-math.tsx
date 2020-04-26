import {Component, Prop, Watch, Element, h, Host, Event, EventEmitter} from '@stencil/core';
import katex from 'katex';

@Component({
  tag: 'deckgo-math',
  styleUrl: 'deckdeckgo-math.scss',
})
export class DeckdeckgoMath {
  @Element() el: HTMLElement;
  @Prop() expression: string;
  @Prop() options: any = {};

  @Watch('options')
  async optionsChanged() {
    this.renderMathExpression();
  }

  @Event() mathError!: EventEmitter<any>;

  async componentDidLoad() {
    this.renderMathExpression();
  }

  private renderMathExpression(): Promise<void> {
    if (!this.expression || this.expression === undefined || this.expression === '') {
      return;
    }
    if (this.expression) {
      try {
        katex.render(this.expression, this.el, {...this.options});
      } catch (error) {
        if (error instanceof katex.ParseError) {
          // KaTeX can't parse the expression
          let message = ("Error in LaTeX '" + this.expression + "': " + error.message).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
          this.mathError.emit(message);
        } else {
          this.mathError.emit(error); //other error
        }
      }
    }
  }

  render() {
    return <Host> </Host>;
  }
}
