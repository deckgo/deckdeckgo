import {Component, Prop, Watch, Element, Method, EventEmitter, Event, Listen, State, h, Host} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';

import {loadTheme} from '../../utils/themes-loader.utils';
import {parseCode} from '../../utils/parse.utils';
import { loadGoogleFonts } from "../../utils/fonts.utils";

import {CarbonThemeStyle} from '../styles/deckdeckgo-highlight-code-theme';
import { HighlightStyle } from "../styles/highlight.style";

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
export class DeckdeckgoHighlightCode {
  @Element() el: HTMLElement;

  /**
   * Emitted when a language is fetched and loaded
   */
  @Event()
  prismLanguageLoaded: EventEmitter<string>;

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
  private languagesToLoad: string[];

  @State()
  private loaded: boolean = false;

  private refContainer!: HTMLDivElement;

  private readonly debounceUpdateSlot: () => void;

  private highlightGroup: number | undefined = undefined;
  private highlightEnd: boolean = false;

  @State()
  private highlightRows: {start: number, end: number} | undefined = undefined;

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
    const languageWasLoaded: boolean = await this.languageDidLoad();

    await this.loadLanguages();

    if (languageWasLoaded) {
      await this.parse();
    }
  }

  async componentDidUpdate() {
    if (this.parseAfterUpdate) {
      await this.parse();
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

  @Listen('prismLanguageLoaded', {target: 'document'})
  async languageLoaded($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    if (this.languagesToLoad) {
      this.languagesToLoad = this.languagesToLoad.filter((lang) => lang !== $event.detail);
    }

    if (this.language && !this.loaded && (this.languagesToLoad === undefined || this.languagesToLoad.length <= 0)) {
      await this.parse();

      this.loaded = true;
    }
  }

  private async parse() {
    if (!this.language || !deckdeckgoHighlightCodeLanguages[this.language]) {
      return;
    }

    await this.parseSlottedCode();
  }

  private languageDidLoad(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!document || !this.language || this.language === '') {
        resolve(false);
        return;
      }

      const scripts = document.querySelector("[deckdeckgo-prism-loaded='" + this.language + "']");
      if (scripts) {
        resolve(true);
      } else {
        resolve(false);
      }
    });
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
    await this.initLanguagesToLoad();

    await this.loadLanguagesRequire();

    await this.loadScript(this.language, reload);
  }

  private async initLanguagesToLoad() {
    if (!this.language) {
      return;
    }

    const definition = deckdeckgoHighlightCodeLanguages[this.language];
    this.languagesToLoad = definition.require && definition.require.length > 0 ? [this.language, ...definition.require] : [this.language];
  }

  private async loadLanguagesRequire() {
    const promises: Promise<void>[] = [];

    const definition = deckdeckgoHighlightCodeLanguages[this.language];
    if (definition.require) {
      promises.push(...definition.require.map((extraScript) => this.loadScript(extraScript, false, true)));
    }

    if (promises.length <= 0) {
      return;
    }

    await Promise.all(promises);
  }

  private loadScript(lang: string, reload: boolean = false, requireScript: boolean = false): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document || !lang || lang === '') {
        resolve();
        return;
      }

      // No need to load javascript, it is there
      if (lang === 'javascript') {
        this.prismLanguageLoaded.emit('javascript');

        resolve();
        return;
      }

      const scripts = document.querySelector("[deckdeckgo-prism='" + lang + "']");
      if (scripts) {
        if (reload) {
          this.prismLanguageLoaded.emit(lang);
        }

        resolve();
        return;
      }

      const script = document.createElement('script');

      script.onload = async () => {
        script.setAttribute('deckdeckgo-prism-loaded', lang);
        this.prismLanguageLoaded.emit(lang);
      };

      script.onerror = async () => {
        if (script.parentElement) {
          script.parentElement.removeChild(script);
        }

        // if the language definition doesn't exist or if unpkg is down, display code anyway
        this.prismLanguageLoaded.emit(lang);
      };
      const definition = deckdeckgoHighlightCodeLanguages[this.language];

      let language = !requireScript && definition.main ? definition.main : lang;

      script.src = 'https://unpkg.com/prismjs@latest/components/prism-' + language + '.js';
      script.setAttribute('deckdeckgo-prism', language);
      script.defer = true;

      document.head.appendChild(script);

      script.addEventListener('load', () => resolve(), {once: true});
    });
  }

  @Watch('lineNumbers')
  async onLineNumbersChange() {
    await this.parse();
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
  load(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.language || this.language === '') {
        resolve();
        return;
      }

      if (this.language === 'javascript') {
        await this.parse();
        resolve();
        return;
      }

      if (document.querySelector("[deckdeckgo-prism-loaded='" + this.language + "']")) {
        await this.parse();
      } else {
        await this.loadLanguages();
      }

      resolve();
    });
  }

  private parseSlottedCode(): Promise<void> {
    const code: HTMLElement = this.el.querySelector("[slot='code']");

    if (code) {
      return parseCode({
        ...this.parseCodeOptions(),
        code: code?.innerHTML?.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')
      });
    }

    return new Promise<void>((resolve) => {
      resolve();
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

    await this.copyCodeToSlot();

    await this.parseSlottedCode();

    this.codeDidChange.emit(this.el);
  }

  private inputCode() {
    if (!this.editable) {
      return;
    }

    this.debounceUpdateSlot();
  }

  private async copyCodeToSlot() {
    const code: HTMLElement | null = this.el.querySelector("[slot='code']");

    if (code) {
      code.innerHTML = this.refCode?.innerText?.replace(/(?:\n\n)/g, '\n').replace(/\u200B/g, '');
    }
  }

  private edit() {
    if (!this.editable) {
      return;
    }

    this.refCode?.focus();
  }

  private catchTab = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Tab') {
      $event.preventDefault();

      document.execCommand('insertHTML', false, '&#009');
    }
  };

  /**
   * Animate highlighted lines and, apply "focus" on next group
   */
  @Method()
  async nextHighlight() {
    if (this.highlightEnd) {
      return;
    }

    await this.selectNextGroupHighlight(this.highlightGroup + 1 || 0);

    // We want to limit the counter to max count of groups
    if (this.highlightRows !== undefined) {
      this.highlightGroup = this.highlightGroup + 1 || 0;
    } else {
      this.highlightEnd = true;
    }
  }

  /**
   * Animate highlighted lines and, apply "focus" on previous group
   */
  @Method()
  async prevHighlight() {
    if (this.highlightGroup === 0) {
      this.highlightGroup = undefined;
      this.highlightRows = undefined;
      return;
    }

    this.highlightGroup = this.highlightEnd ? this.highlightGroup : this.highlightGroup - 1;

    await this.selectNextGroupHighlight(this.highlightGroup);

    if (this.highlightRows !== undefined) {
      this.highlightEnd = false;
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
      end: allRows.indexOf(rows[rows.length - 1]),
    }
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
        <div class="deckgo-highlight-code-container" ref={(el: HTMLDivElement | null) => (this.refContainer = el as HTMLDivElement)}>
          <code class={this.highlightLines?.length > 0 ? 'highlight' : undefined}
            contentEditable={this.editable}
            onBlur={async () => await this.applyCode()}
            onInput={() => this.inputCode()}
            onKeyDown={($event: KeyboardEvent) => this.catchTab($event)}
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
