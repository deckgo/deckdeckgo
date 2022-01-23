import {Component, h, ComponentInterface, Host, Method, Prop, Element} from '@stencil/core';

import * as monaco from 'monaco-editor';

import {MonacoEditorOptions} from '../types/options';

@Component({
  tag: 'deckgo-monaco-editor',
  styleUrl: 'monaco-editor.scss',
  shadow: true
})
export class MonacoEditor implements ComponentInterface {
  @Element()
  private el: HTMLElement;

  @Prop()
  options: MonacoEditorOptions;

  private editor?: monaco.editor.IStandaloneCodeEditor;

  private div!: HTMLDivElement;

  private readonly defaultOptions: monaco.editor.IStandaloneEditorConstructionOptions = {
    language: 'javascript',

    scrollBeyondLastLine: false,
    readOnly: false,
    theme: 'vs-light',

    minimap: {
      enabled: false
    },

    automaticLayout: true
  };

  async componentDidLoad() {
    const slottedCode: HTMLElement = this.el.querySelector(':scope > *:first-of-type');

    this.editor = monaco.editor.create(this.div, {
      value: slottedCode?.innerHTML.trim() || '',
      ...this.defaultOptions,
      ...(this.options || {})
    });
  }

  disconnectedCallback() {
    this.editor?.dispose();
  }

  @Method()
  async save(): Promise<string | undefined> {
    return this.editor?.getValue();
  }

  render() {
    return (
      <Host>
        <article ref={(el) => (this.div = el as HTMLDivElement)}></article>
      </Host>
    );
  }
}
