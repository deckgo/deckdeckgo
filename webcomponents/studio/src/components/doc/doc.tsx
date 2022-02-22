import {moveCursorToStart} from '@deckdeckgo/utils';
import {isFirefox} from '@deckdeckgo/utils/lib';
import { Component, ComponentInterface, Element, h, Host, JSX, Method, Prop, State, Watch } from '@stencil/core';
import {nanoid} from 'nanoid';
import {ChartEvents} from '../../events/chart/chart.events';
import {DocDataEvents} from '../../events/doc/doc.data.events';
import {DocImageEvents} from '../../events/doc/doc.image.events';
import {ImageEvents} from '../../events/image/image.events';
import {ParagraphHelper} from '../../helpers/paragraph-helper';
import busyStore from '../../stores/busy.store';
import configStore from '../../stores/config.store';
import editorStore from '../../stores/editor.store';
import i18nStore from '../../stores/i18n.store';
import {StudioConfig} from '../../types/config';
import {Editor} from '../../types/editor';
import {getEdit} from '../../utils/editor.utils';

@Component({
  tag: 'deckgo-studio-doc',
  styleUrl: 'doc.scss',
  shadow: false
})
export class Doc implements ComponentInterface {
  @Element()
  private el: HTMLElement;

  @State()
  private paragraphs: JSX.IntrinsicElements[] = [];

  @Prop()
  studioConfig: StudioConfig;

  private readonly imageEvents: ImageEvents = new ImageEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly docDataEvents: DocDataEvents = new DocDataEvents();
  private readonly docImageEvents: DocImageEvents = new DocImageEvents();

  private readonly paragraphHelper: ParagraphHelper = new ParagraphHelper();

  private containerRef!: HTMLElement;
  private styloEditorRef!: HTMLStyloEditorElement;

  // Hack: we need to clean DOM first on reload as we mix both intrinsect elements and dom elements (content editable)
  private reloadAfterRender: boolean = false;

  // TODO: config including i18n

  componentWillLoad() {
    this.applyConfig();
  }

  async componentDidLoad() {
    this.imageEvents.init();
    this.chartEvents.init();
    // TODO: code events

    this.docImageEvents.init(this.containerRef);

    await this.initOrFetch();
  }

  async disconnectedCallback() {
    this.imageEvents.destroy();
    this.chartEvents.destroy();
    // TODO: code events

    this.docImageEvents.destroy();

    this.destroy();

    // TODO: i18n
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
    this.docDataEvents.destroy();

    editorStore.reset();
  }

  @Watch('studioConfig')
  onConfigChange() {
    configStore.reset();

    this.applyConfig();
  }

  private applyConfig() {
    if (!this.studioConfig) {
      return;
    }

    const {cloud, lang, stylo} = this.studioConfig;
    configStore.state.cloud = cloud;
    configStore.state.stylo = stylo;
    i18nStore.state.lang = lang;
  }

  @Method()
  async initNewDoc() {
    this.destroy();

    this.resetDOM();

    this.reloadAfterRender = true;
    this.paragraphs = undefined;
  }

  private async reload() {
    await this.initOrFetch();

    // Reset config will destroy and init again listener in Stylo. It also reset undo-redo stack.
    configStore.state.stylo = {...configStore.state.stylo};
  }

  private async initOrFetch() {
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
    /**
     * Pragmatic hack for Firefox: we cannot use the css selector :empty in Firefox because if user enter text in a first empty title, the text is not added within the title but before it.
     */
    const firefox: boolean = isFirefox();

    const Title = 'h1';
    const title: JSX.IntrinsicElements = <Title key={nanoid()}>{firefox ? '\u200B' : undefined}</Title>;

    const Div = 'div';
    const div: JSX.IntrinsicElements = <Div key={nanoid()}></Div>;

    this.paragraphs = firefox ? [title] : [title, div];

    busyStore.state.docReady = true;
  }

  private async fetchDoc(docId: string) {
    const paragraphs: JSX.IntrinsicElements[] = await this.paragraphHelper.loadDocAndRetrieveParagraphs(docId);
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
    this.docDataEvents.init();

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
        <deckgo-doc>
          <article contentEditable={true} ref={(el) => (this.containerRef = el as HTMLElement)}>
            {this.paragraphs}
          </article>

          <stylo-editor ref={(el) => (this.styloEditorRef = el as HTMLStyloEditorElement)} config={configStore.state.stylo}></stylo-editor>
        </deckgo-doc>

        <deckgo-doc-indicator></deckgo-doc-indicator>
      </Host>
    );
  }
}
