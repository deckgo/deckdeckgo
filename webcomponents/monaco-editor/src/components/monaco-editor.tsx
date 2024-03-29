import {Component, ComponentInterface, Element, Event, EventEmitter, h, Host, Method, Prop, Watch} from '@stencil/core';
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
      ...this.mergeOptions()
    });

    this.editorDidLoad.emit();
  }

  async disconnectedCallback() {
    await this.dispose();
  }

  private dispose(): Promise<void> {
    if (!this.editor) {
      return Promise.resolve();
    }

    return new Promise<void>((resolve) => {
      this.editor.onDidDispose(() => resolve());

      this.editor.getModel()?.dispose();
      this.editor.dispose();
    });
  }

  @Watch('options')
  onOptionsChange() {
    this.editor?.updateOptions(this.mergeOptions());
  }

  @Method()
  async setFocus() {
    this.editor?.focus();
  }

  @Method()
  async updateLanguage(languageId: string) {
    monaco.editor.setModelLanguage(this.editor?.getModel(), languageId);
  }

  @Method()
  async save(): Promise<string | undefined> {
    return escapeCode(this.editor?.getValue());
  }

  private mergeOptions(): monaco.editor.IStandaloneEditorConstructionOptions {
    return {
      ...this.defaultOptions,
      ...(this.options || {})
    };
  }

  render() {
    return (
      <Host>
        <article ref={(el) => (this.div = el as HTMLDivElement)}></article>
      </Host>
    );
  }
}
