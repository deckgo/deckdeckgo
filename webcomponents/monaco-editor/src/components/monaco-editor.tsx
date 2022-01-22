import {Component, Element, h, ComponentInterface, Host, Method} from '@stencil/core';

import * as monaco from 'monaco-editor';

@Component({
  tag: 'deckgo-monaco-editor',
  styleUrl: 'monaco-editor.scss',
  shadow: true
})
export class MonacoEditor implements ComponentInterface {
  @Element()
  private el: HTMLElement;

  private editor?: monaco.editor.IStandaloneCodeEditor;

  async componentDidLoad() {
    const div = this.el.shadowRoot.querySelector('main');

    this.editor = monaco.editor.create(div, {
      value: "// First line\nfunction hello() {\n\talert('Hello world!');\n}\n// Last line",
      language: 'javascript',

      scrollBeyondLastLine: false,
      readOnly: false,
      theme: 'vs-light',

      minimap: {
        enabled: false
      },

      automaticLayout: true
    });
  }

  disconnectedCallback() {
    this.editor?.dispose();
  }

  @Method()
  async save(): Promise<string> {
    return this.editor?.getValue();
  }

  render() {
    return (
      <Host>
        <main></main>
      </Host>
    );
  }
}
