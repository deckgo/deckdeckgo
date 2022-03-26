import {Doc} from '@deckdeckgo/editor';
import {Editor, getEdit} from '@deckdeckgo/offline';
import {moveCursorToStart} from '@deckdeckgo/utils';
import {StyloConfig} from '@papyrs/stylo';
import {Component, ComponentInterface, Element, Event, EventEmitter, h, Host, JSX, Method, Prop, State, Watch} from '@stencil/core';
import {nanoid} from 'nanoid';
import {loadDocAndRetrieveParagraphs} from '../../helpers/paragraph-helper';
import i18nStore from '../../stores/i18n.store';
import busyStore from '../../stores/ready.store';
import readyStore from '../../stores/ready.store';

@Component({
  tag: 'deckgo-studio-doc',
  styleUrl: 'doc.scss',
  shadow: false
})
export class StudioDoc implements ComponentInterface {
  @Element()
  private el: HTMLElement;

  @State()
  private paragraphs: JSX.IntrinsicElements[] = [];

  @Prop()
  styloConfig: Partial<StyloConfig>;

  @Prop()
  loadDoc: (docId: string | undefined) => Promise<Doc> | undefined;

  @Prop()
  resetDoc: () => void | undefined;

  @Event()
  docDidLoad: EventEmitter<HTMLElement>;

  @Event()
  docReady: EventEmitter<boolean>;

  @Event()
  docDataEvents: EventEmitter<'init' | 'destroy'>;

  @State()
  private config: Partial<StyloConfig> = {};

  private containerRef!: HTMLElement;
  private styloEditorRef!: HTMLStyloEditorElement;

  // Hack: we need to clean DOM first on reload as we mix both intrinsect elements and dom elements (content editable)
  private reloadAfterRender: boolean = false;

  private unsubscribe: () => void | undefined;

  componentWillLoad() {
    this.applyConfig();

    this.unsubscribe = busyStore.onChange('docReady', (docReady: boolean) => this.docReady.emit(docReady));
  }

  async componentDidLoad() {
    this.docDidLoad.emit(this.containerRef);

    await this.initOrFetch();
  }

  async disconnectedCallback() {
    this.destroy();

    this.unsubscribe?.();
  }

  async componentDidRender() {
    if (!this.reloadAfterRender) {
      return;
    }

    this.reloadAfterRender = false;

    await this.reload();
  }

  /**
   * Destroy global state and listener.
   */
  private destroy() {
    this.docDataEvents.emit('destroy');

    readyStore.reset();

    this.resetDoc?.();
  }

  @Watch('styloConfig')
  onConfigChange() {
    this.config = {};

    this.applyConfig();
  }

  private applyConfig() {
    if (!this.styloConfig) {
      return;
    }

    this.config = this.styloConfig;

    const {i18n} = this.styloConfig;
    i18nStore.state.lang = i18n?.lang || 'en';
  }

  @Method()
  async init() {
    this.destroy();

    this.resetDOM();

    this.reloadAfterRender = true;
    this.paragraphs = undefined;
  }

  private async reload() {
    await this.initOrFetch();

    // Reset config will destroy and init again listener in Stylo. It also reset undo-redo stack.
    this.config = {...this.config};
  }

  private async initOrFetch() {
    // If not provided then developer will have to call manually init
    if (!this.loadDoc || !this.resetDoc) {
      return;
    }

    const editor: Editor | undefined = await getEdit();
    const docId: string | undefined = editor?.id;

    this.initDocDataEvents(!docId);
    this.initFocus();
    this.initContainerRef();

    if (!docId) {
      await this.initDoc();
    } else {
      await this.fetchDoc(docId);
    }
  }

  private async initDoc() {
    const Title = 'h1';
    const title: JSX.IntrinsicElements = <Title key={nanoid()}></Title>;

    const Div = 'div';
    const div: JSX.IntrinsicElements = <Div key={nanoid()}></Div>;

    this.paragraphs = [title, div];

    busyStore.state.docReady = true;
  }

  private async fetchDoc(docId: string) {
    const paragraphs: JSX.IntrinsicElements[] = await loadDocAndRetrieveParagraphs({docId, loadDoc: this.loadDoc});
    this.paragraphs = paragraphs?.length > 0 ? [...paragraphs] : [];
  }

  private resetDOM() {
    if (!this.containerRef) {
      return;
    }

    this.containerRef.innerHTML = '';
  }

  // If we init, we observe the default elements. When rendered, we simulate
  private initDocDataEvents(init: boolean) {
    this.docDataEvents.emit('init');

    if (!init) {
      return;
    }

    const onRender = (mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      const addedParagraphs: Node[] = mutations
        .filter(({addedNodes}: MutationRecord) => addedNodes?.length > 0)
        .reduce((acc: Node[], {addedNodes}: MutationRecord) => [...acc, ...Array.from(addedNodes)], []);

      // Same event as in stylo
      const $event: CustomEvent<Node[]> = new CustomEvent<Node[]>('addParagraphs', {detail: addedParagraphs, bubbles: true});
      this.el.dispatchEvent($event);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  // Init only once the elements within the articles are rendered otherwise they will be added to the undo-redo stack
  private initContainerRef() {
    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      this.styloEditorRef.containerRef = this.containerRef;
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  private initFocus() {
    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      moveCursorToStart(this.containerRef?.firstChild?.firstChild);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  render() {
    return (
      <Host>
        <article contentEditable={true} ref={(el) => (this.containerRef = el as HTMLElement)} class="deckgo-doc">
          {this.paragraphs}
        </article>

        <stylo-editor ref={(el) => (this.styloEditorRef = el as HTMLStyloEditorElement)} config={this.config}></stylo-editor>
      </Host>
    );
  }
}
