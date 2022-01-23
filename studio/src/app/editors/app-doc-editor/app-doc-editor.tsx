import {Component, ComponentInterface, Element, Fragment, h, JSX, Listen, Method, State} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import {modalController} from '@ionic/core';

import {isFirefox, moveCursorToStart} from '@deckdeckgo/utils';
import {StyloConfig, h1, h2, h3, ul, StyloPaletteColor} from '@deckdeckgo/stylo';

import editorStore from '../../stores/editor.store';
import busyStore from '../../stores/busy.store';
import undoRedoStore from '../../stores/undo-redo.store';
import errorStore from '../../stores/error.store';
import authStore from '../../stores/auth.store';
import colorStore from '../../stores/color.store';
import i18n from '../../stores/i18n.store';

import {Editor} from '../../types/editor/editor';
import {SlotType} from '../../types/editor/slot-type';

import {ImageEvents} from '../../events/core/image/image.events';
import {ChartEvents} from '../../events/core/chart/chart.events';
import {DocDataEvents} from '../../events/editor/doc/doc.data.events';
import {DocImageEvents} from '../../events/editor/doc/doc.image.events';
import {CodeEvents} from '../../events/editor/code/code.events';

import {ParagraphHelper} from '../../helpers/editor/paragraphHelper';

import {getEdit} from '../../utils/editor/editor.utils';
import {printDoc} from '../../utils/editor/print.utils';
import {cloud} from '../../utils/core/environment.utils';
import {signIn} from '../../utils/core/signin.utils';
import {ColorUtils} from '../../utils/editor/color.utils';

import {imgStorage} from '../../plugins/img.storage.plugin';
import {imgUnsplash} from '../../plugins/img.unsplash.plugin';
import {imgGif} from '../../plugins/img.gif.plugin';
import {hr} from '../../plugins/hr.plugin';
import {code} from '../../plugins/code.plugin';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  @Element()
  private el: HTMLElement;

  @State()
  private paragraphs: JSX.IntrinsicElements[] = [];

  @State()
  private editorConfig: Partial<StyloConfig> = {
    plugins: [h1, h2, h3, ul, imgStorage, imgUnsplash, imgGif, code, hr]
  };

  private readonly imageEvents: ImageEvents = new ImageEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly docDataEvents: DocDataEvents = new DocDataEvents();
  private readonly docImageEvents: DocImageEvents = new DocImageEvents();
  private readonly codeEvents: CodeEvents = new CodeEvents();

  private readonly paragraphHelper: ParagraphHelper = new ParagraphHelper();

  private containerRef!: HTMLElement;
  private styleEditorRef!: HTMLStyloEditorElement;

  // Hack: we need to clean DOM first on reload as we mix both intrinsect elements and dom elements (content editable)
  private reloadAfterRender: boolean = false;

  componentWillLoad() {
    this.updateEditorToolbarConfig();
  }

  async componentDidLoad() {
    this.imageEvents.init();
    this.chartEvents.init();
    this.codeEvents.init();

    this.docImageEvents.init(this.containerRef);

    await this.initOrFetch();
  }

  async disconnectedCallback() {
    this.imageEvents.destroy();
    this.chartEvents.destroy();
    this.codeEvents.destroy();

    this.docImageEvents.destroy();

    this.destroy();
  }

  /**
   * Destroy global state and listener.
   */
  private destroy() {
    this.docDataEvents.destroy();

    editorStore.reset();
    undoRedoStore.reset();
  }

  @Listen('actionPublish', {target: 'document'})
  async onActionPublish() {
    if (!cloud()) {
      errorStore.state.error = 'No cloud provider to publish material.';
      return;
    }

    if (!authStore.state.authUser) {
      signIn();
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-publish',
      cssClass: 'fullscreen'
    });

    await modal.present();
  }

  @Listen('keydown', {target: 'document'})
  onKeyDown($event: KeyboardEvent) {
    const {key, ctrlKey, metaKey} = $event;

    if (key === 'p' && (ctrlKey || metaKey)) {
      this.print($event);
    }
  }

  @Listen('colorChange', {target: 'document', passive: true})
  onColorChange({detail}: CustomEvent<StyloPaletteColor>) {
    ColorUtils.updateColor(detail);

    this.updateEditorToolbarConfig();
  }

  private updateEditorToolbarConfig() {
    this.editorConfig = {
      ...this.editorConfig,
      lang: i18n.state.lang,
      toolbar: {palette: colorStore.state.history.slice(0, 11)}
    };
  }

  private print($event: KeyboardEvent) {
    if (!this.containerRef) {
      return;
    }

    $event.preventDefault();

    printDoc({element: this.containerRef});
  }

  @Method()
  async initNewDoc() {
    this.destroy();

    this.resetDOM();

    this.reloadAfterRender = true;
    this.paragraphs = undefined;
  }

  async componentDidRender() {
    if (!this.reloadAfterRender) {
      return;
    }

    this.reloadAfterRender = false;

    await this.reload();
  }

  private async reload() {
    await this.initOrFetch();
  }

  private async initOrFetch() {
    const editor: Editor | undefined = await getEdit();
    const docId: string | undefined = editor?.id;

    this.initDocDataEvents(!docId);
    this.initEditable();
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
    const title: JSX.IntrinsicElements = <Title key={uuid()}>{firefox ? '\u200B' : undefined}</Title>;

    const Div = 'div';
    const div: JSX.IntrinsicElements = <Div key={uuid()}></Div>;

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

      this.styleEditorRef.containerRef = this.containerRef;
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  private initEditable() {
    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      const elements: NodeListOf<HTMLElement> = this.containerRef.querySelectorAll<HTMLElement>(
        `${SlotType.CODE}, ${SlotType.MATH}, ${SlotType.WORD_CLOUD}, ${SlotType.MARKDOWN}`
      );
      Array.from(elements).forEach((element: HTMLElement) => element.setAttribute('editable', 'true'));
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
      <Fragment>
        <ion-content class={`ion-no-padding`}>
          <main>
            {this.renderLoading()}

            <deckgo-doc>
              <article contentEditable={true} ref={(el) => (this.containerRef = el as HTMLElement)}>
                {this.paragraphs}
              </article>

              <stylo-editor ref={(el) => (this.styleEditorRef = el as HTMLStyloEditorElement)} config={this.editorConfig}></stylo-editor>
            </deckgo-doc>

            <app-doc-indicator></app-doc-indicator>
          </main>
        </ion-content>
      </Fragment>
    );
  }

  private renderLoading() {
    if (busyStore.state.docReady) {
      return undefined;
    }

    return <app-spinner></app-spinner>;
  }
}
