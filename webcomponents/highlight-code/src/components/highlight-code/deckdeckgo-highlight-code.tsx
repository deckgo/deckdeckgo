import {Component, Prop, Watch, Element, Method, EventEmitter, Event, Listen, State, h, Host} from '@stencil/core';

import {catchTab, debounce, getSelection, moveCursorToEnd} from '@deckdeckgo/utils';
import {DeckDeckGoRevealComponent} from '@deckdeckgo/slide-utils';

import {loadTheme} from '../../utils/themes-loader.utils';
import {parseCode} from '../../utils/parse.utils';
import {loadGoogleFonts} from '../../utils/fonts.utils';
import {injectRequiredJS, loadMainScript, StateRequiredJS} from '../../utils/inject.utils';

import {CarbonThemeStyle} from '../styles/carbon-theme.style';
import {HighlightStyle} from '../styles/highlight.style';

import {DeckdeckgoHighlightCodeCarbonTheme} from '../../declarations/deckdeckgo-highlight-code-carbon-theme';
import {DeckdeckgoHighlightCodeTerminal} from '../../declarations/deckdeckgo-highlight-code-terminal';
import {deckdeckgoHighlightCodeLanguages} from '../../declarations/deckdeckgo-highlight-code-languages';

/**
 * @slot code - A `<code/>` element to highlight
 * @slot user - A user name for the Ubuntu terminal
 */
@Component({
  tag: 'deckgo-highlight-code',
  styleUrl: 'deckdeckgo-highlight-code.scss',
  shadow: true
})
export class DeckdeckgoHighlightCode implements DeckDeckGoRevealComponent {
  @Element() el: HTMLElement;

  /**
   * Emitted when a language is fetched and loaded
   */
  @Event()
  prismLanguageLoaded: EventEmitter<string>;

  /**
   * Emitted when a language could not be loaded. The component fallback to javascript language to display the code anyway.
   */
  @Event()
  prismLanguageError: EventEmitter<string>;

  /**
   * Emitted when the code was edited (see attribute editable). Propagate the root component itself
   */
  @Event()
  codeDidChange: EventEmitter<HTMLElement>;

  /**
   * Define the language to be used for the syntax highlighting. The list of supported languages is defined by Prism.js
   */
  @Prop({reflect: true}) language: string = 'javascript';

  /**
   * If you wish to highlight some lines of your code. The lines number should be provided as a number (one line) or numbers separated with coma or dash (many lines), group separated with space.
   * For example: 1 3,5 8 14-17 which highlight lines  1, 3 to 5, 8 and 14 to 17
   */
  @Prop({reflect: true}) highlightLines: string;
  /**
   * Display the number of the lines of code
   */
  @Prop({reflect: true}) lineNumbers: boolean = false;

  /**
   * Present the code in a stylish "windowed" card
   */
  @Prop({reflect: true}) terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;

  /**
   * In case you would like to set the code component as being editable
   */
  @Prop() editable: boolean = false;

  /**
   * The theme of the selected terminal (applied only in case of carbon)
   */
  @Prop({reflect: true}) theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

  private parseAfterUpdate: boolean = false;

  private refCode!: HTMLElement;

  @State()
  private themeStyle: string | undefined;

  @State()
  private loaded: boolean = false;

  private refContainer!: HTMLDivElement;

  private readonly debounceUpdateSlot: () => void;

  private highlightGroup: number | undefined = undefined;

  /**
   * @internal Used when integrated in DeckDeckGo to display next and previous highlight in the presentations
   */
  @Prop({mutable: true})
  revealProgress: 'start' | 'partial' | 'end' = 'start';

  @State()
  private highlightRows: {start: number; end: number} | undefined = undefined;

  private editFocused: boolean = false;

  constructor() {
    this.debounceUpdateSlot = debounce(async () => {
      await this.copyCodeToSlot();
    }, 500);
  }

  async componentWillLoad() {
    await loadGoogleFonts(this.terminal);

    await this.loadTheme();
  }

  async componentDidLoad() {
    const languageWasLoaded: boolean = this.languageDidLoad();

    await this.loadLanguages();

    if (languageWasLoaded) {
      this.parse();
    }
  }

  componentDidUpdate() {
    if (this.parseAfterUpdate) {
      this.parse();
      this.parseAfterUpdate = false;
    }
  }

  @Watch('theme')
  async loadTheme() {
    if (this.terminal !== DeckdeckgoHighlightCodeTerminal.CARBON || !this.theme) {
      this.themeStyle = undefined;
      return;
    }

    const {theme} = await loadTheme(this.theme);
    this.themeStyle = theme;
  }

  @Listen('prismLanguageLoaded', {target: 'document', passive: true})
  onLanguageLoaded({detail}: CustomEvent<string>) {
    if (this.language !== detail || this.loaded) {
      return;
    }

    this.parse();

    this.loaded = true;
  }

  @Listen('prismLanguageError', {target: 'document', passive: true})
  async onLanguageError({detail}: CustomEvent<string>) {
    if (this.language !== detail) {
      return;
    }

    this.language = 'javascript';
    this.prismLanguageLoaded.emit(this.language);
  }

  private parse() {
    if (!this.language || !deckdeckgoHighlightCodeLanguages[this.language]) {
      return;
    }

    this.parseSlottedCode();
  }

  private languageDidLoad(): boolean {
    if (!document || !this.language || this.language === '') {
      return false;
    }

    const scripts: HTMLScriptElement | null = document.querySelector("[deckdeckgo-prism-loaded='" + this.language + "']");
    if (scripts) {
      return true;
    }

    return false;
  }

  @Watch('language')
  async onLanguage() {
    await this.loadLanguages(true);
  }

  private async loadLanguages(reload: boolean = false) {
    this.loaded = false;

    if (!this.language || !deckdeckgoHighlightCodeLanguages[this.language]) {
      console.error(`Language ${this.language} is not supported`);
      return;
    }

    const loadingScript: 'attached' | 'loaded' | 'error' = await this.loadRequiredLanguages();

    // We need all required scripts to be loaded. If multiple components are use within the same page, it is possible that the required scripts are attached to the DOM and are still loading.
    // loadScript will trigger an event on the document, therefore those who do not loadScript will receive the event anyway when everything is ready.
    if (loadingScript === 'attached') {
      return;
    }

    if (loadingScript === 'error') {
      this.fallbackJavascript();
      return;
    }

    const state: 'loaded' | 'error' = await loadMainScript({lang: this.language, reload, prismLanguageLoaded: this.prismLanguageLoaded});

    if (state === 'loaded') {
      return;
    }

    this.fallbackJavascript();
  }

  private fallbackJavascript() {
    console.error('A required script for the language could not be fetched therefore, falling back to JavaScript to display code anyway.');
    this.prismLanguageError.emit(this.language);
  }

  private async loadRequiredLanguages(): Promise<'attached' | 'loaded' | 'error'> {
    if (!this.language) {
      return 'error';
    }

    const definition = deckdeckgoHighlightCodeLanguages[this.language];

    if (!definition.require || definition.require.length <= 0) {
      return 'loaded';
    }

    // Load now the required languages scripts because Prism needs these to be loaded before the actual main language script
    const promises: Promise<StateRequiredJS>[] = definition.require.map((lang: string) => injectRequiredJS({lang}));
    const states: StateRequiredJS[] = await Promise.all(promises);

    const stateError: StateRequiredJS | undefined = states.find((state: StateRequiredJS) => ['error', 'abort'].includes(state));
    if (stateError !== undefined) {
      return 'error';
    }

    const stateNotLoaded: StateRequiredJS | undefined = states.find((state: StateRequiredJS) => state !== 'loaded');
    return stateNotLoaded !== undefined ? 'attached' : 'loaded';
  }

  @Watch('lineNumbers')
  onLineNumbersChange() {
    this.parse();
  }

  @Watch('terminal')
  async onCarbonChange() {
    this.parseAfterUpdate = true;

    await loadGoogleFonts(this.terminal);
  }

  /**
   * Load or reload the component
   */
  @Method()
  async load() {
    if (!this.language || this.language === '') {
      return;
    }

    if (this.language === 'javascript') {
      this.parse();
      return;
    }

    if (document.querySelector("[deckdeckgo-prism-loaded='" + this.language + "']")) {
      this.parse();
    } else {
      await this.loadLanguages();
    }
  }

  private parseSlottedCode() {
    const code: HTMLElement = this.el.querySelector("[slot='code']");

    if (!code) {
      return;
    }

    parseCode({
      ...this.parseCodeOptions(),
      code: code?.innerHTML?.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')
    });
  }

  private parseCodeOptions() {
    return {
      refContainer: this.refContainer,
      refCode: this.refCode,
      lineNumbers: this.lineNumbers,
      highlightLines: this.highlightLines,
      language: this.language
    };
  }

  private async applyCode() {
    if (!this.editable) {
      return;
    }

    this.editFocused = false;

    await this.copyCodeToSlot();

    this.parseSlottedCode();

    this.codeDidChange.emit(this.el);
  }

  private inputCode() {
    if (!this.editable) {
      return;
    }

    this.debounceUpdateSlot();
  }

  private async copyCodeToSlot() {
    const code: HTMLElement | null = this.el.querySelector(":scope > [slot='code']");

    if (!code) {
      return;
    }

    // Avoid duplicating new lines on new entries
    this.refCode?.querySelectorAll('br')?.forEach((node: HTMLBRElement) => (node.outerHTML = '\u200B'));

    code.innerHTML = this.refCode?.innerText
      .replace(/\u200B/g, '')
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  private edit() {
    if (!this.editable || this.editFocused) {
      return;
    }

    if (!this.refCode) {
      return;
    }

    this.editFocused = true;

    this.refCode.focus();
    moveCursorToEnd(this.refCode);
  }

  /**
   * @internal Used when integrated in DeckDeckGo presentations. Call `nextHighlight()`.
   */
  @Method()
  async reveal() {
    await this.nextHighlight();
  }

  /**
   * @internal Used when integrated in DeckDeckGo presentations. Call `prevHighlight()`.
   */
  @Method()
  async hide() {
    await this.prevHighlight();
  }

  /**
   * @internal Reset the highlight state to default.
   */
  @Method()
  async revealAll() {
    this.highlightGroup = undefined;
    this.highlightRows = undefined;
    this.revealProgress = 'start';
  }

  /**
   * @internal Reset the highlight state to default.
   */
  @Method()
  async hideAll() {
    await this.revealAll();
  }

  /**
   * Animate highlighted lines and, apply "focus" on next group
   */
  @Method()
  async nextHighlight() {
    if (this.revealProgress === 'end') {
      return;
    }

    await this.selectNextGroupHighlight(this.highlightGroup + 1 || 0);

    // We want to limit the counter to max count of groups
    if (this.highlightRows !== undefined) {
      this.highlightGroup = this.highlightGroup + 1 || 0;

      this.revealProgress = 'partial';
      return;
    }

    this.revealProgress = 'end';
  }

  /**
   * Animate highlighted lines and, apply "focus" on previous group
   */
  @Method()
  async prevHighlight() {
    if (this.highlightGroup === 0) {
      this.highlightGroup = undefined;
      this.highlightRows = undefined;
      this.revealProgress = 'start';
      return;
    }

    this.highlightGroup = this.revealProgress === 'end' ? this.highlightGroup : this.highlightGroup - 1;

    await this.selectNextGroupHighlight(this.highlightGroup);

    if (this.highlightRows !== undefined) {
      this.revealProgress = 'partial';
    }
  }

  private async selectNextGroupHighlight(highlightGroup: number | undefined) {
    const rows: NodeListOf<HTMLDivElement> = this.refCode?.querySelectorAll(`.group-${highlightGroup}`);

    if (!rows || rows.length <= 0) {
      this.highlightRows = undefined;
      return;
    }

    const allRows = Array.from(this.refCode.children);

    this.highlightRows = {
      start: allRows.indexOf(rows[0]),
      end: allRows.indexOf(rows[rows.length - 1])
    };
  }

  @Listen('copy', {target: 'window'})
  onCopyCleanZeroWidthSpaces($event: ClipboardEvent) {
    const {target, clipboardData} = $event;

    if (!target || !clipboardData || !this.el.isEqualNode(target as Node)) {
      return;
    }

    const selection: Selection | null = getSelection();

    if (!selection) {
      return;
    }

    $event.preventDefault();

    const text: string = selection.toString().replace(/\u200B/g, '');
    clipboardData.setData('text/plain', text);
  }

  render() {
    const hostClass = {
      'deckgo-highlight-code-carbon': this.terminal === DeckdeckgoHighlightCodeTerminal.CARBON,
      'deckgo-highlight-code-ubuntu': this.terminal === DeckdeckgoHighlightCodeTerminal.UBUNTU
    };

    if (this.terminal === DeckdeckgoHighlightCodeTerminal.CARBON) {
      hostClass[`deckgo-highlight-code-theme-${this.theme}`] = true;
    }

    return (
      <Host class={hostClass} onClick={() => this.edit()}>
        {this.renderCarbon()}
        {this.renderUbuntu()}
        {this.renderHighlightStyle()}
        <div class="container" ref={(el: HTMLDivElement | null) => (this.refContainer = el as HTMLDivElement)}>
          <code
            class={this.highlightLines?.length > 0 ? 'highlight' : undefined}
            contentEditable={this.editable}
            onBlur={async () => await this.applyCode()}
            onInput={() => this.inputCode()}
            onKeyDown={($event: KeyboardEvent) => catchTab($event)}
            ref={(el: HTMLElement | null) => (this.refCode = el as HTMLElement)}></code>
          <slot name="code"></slot>
        </div>
      </Host>
    );
  }

  private renderHighlightStyle() {
    if (!this.highlightLines || this.highlightLines.length <= 0) {
      return undefined;
    }

    return <HighlightStyle {...this.highlightRows} />;
  }

  private renderCarbon() {
    if (this.terminal !== DeckdeckgoHighlightCodeTerminal.CARBON) {
      return undefined;
    }

    return [
      <CarbonThemeStyle style={this.themeStyle} />,
      <div class="carbon">
        {this.renderCarbonCircle('red')}
        {this.renderCarbonCircle('yellow')}
        {this.renderCarbonCircle('green')}
      </div>
    ];
  }

  private renderCarbonCircle(color: 'red' | 'yellow' | 'green') {
    return <div class={color}></div>;
  }

  private renderUbuntu() {
    if (this.terminal !== DeckdeckgoHighlightCodeTerminal.UBUNTU) {
      return undefined;
    }

    return (
      <div class="ubuntu">
        {this.renderUbuntuCircle('close')}
        {this.renderUbuntuCircle('minimize')}
        {this.renderUbuntuCircle('maximize')}
        <p>
          <slot name="user"></slot>
        </p>
      </div>
    );
  }

  private renderUbuntuCircle(mode: 'close' | 'minimize' | 'maximize') {
    const symbol: string = mode === 'close' ? '&#10005;' : mode === 'minimize' ? '&#9472;' : '&#9723;';

    return (
      <div class={mode}>
        <span innerHTML={symbol}></span>
      </div>
    );
  }
}
