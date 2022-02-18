import {Component, h, ComponentInterface, Host, Method, Prop, Element, EventEmitter, Event} from '@stencil/core';

import * as monaco from 'monaco-editor';

import {MonacoEditorOptions} from '../types/options';

import {escapeCode, unescapeCode} from '../utils/code.utils';

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

  @Event()
  editorDidLoad: EventEmitter<void>;

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

  componentDidLoad() {
    const slottedCode: HTMLElement = this.el.querySelector(':scope > *:first-of-type');

    this.editor = monaco.editor.create(this.div, {
      value: unescapeCode(slottedCode?.innerHTML.trim() || ''),
      ...this.defaultOptions,
      ...(this.options || {})
    });

    this.editorDidLoad.emit();
  }

  disconnectedCallback() {
    this.editor?.dispose();
  }

  @Method()
  async setFocus() {
    this.editor?.focus();
  }

  @Method()
  async save(): Promise<string | undefined> {
    return escapeCode(this.editor?.getValue());
  }

  render() {
    return (
      <Host>
        <article ref={(el) => (this.div = el as HTMLDivElement)}></article>
      </Host>
    );
  }
}
