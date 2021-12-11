import {Component, ComponentInterface, Fragment, h, JSX, Listen, Method, State} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import {isFirefox, moveCursorToStart} from '@deckdeckgo/utils';

import colorStore from '../../stores/color.store';
import editorStore from '../../stores/editor.store';
import busyStore from '../../stores/busy.store';
import undoRedoStore from '../../stores/undo-redo.store';

import {Editor} from '../../types/editor/editor';
import {SlotType} from '../../types/editor/slot-type';

import {ImageEvents} from '../../events/core/image/image.events';
import {ChartEvents} from '../../events/core/chart/chart.events';
import {DocDataEvents} from '../../events/editor/doc/doc.data.events';
import {DocEditorEvents} from '../../events/editor/editor/doc.editor.events';
import {DocUndoRedoEvents} from '../../events/editor/doc/doc.undo-redo.events';

import {ParagraphHelper} from '../../helpers/editor/paragraphHelper';

import {getEdit} from '../../utils/editor/editor.utils';
import {printDoc} from '../../utils/editor/print.utils';

import {AppActionsDocEditor} from '../../components/editor/doc/app-actions-doc-editor/app-actions-doc-editor';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  @State()
  private paragraphs: JSX.IntrinsicElements[] = [];

  private readonly imageEvents: ImageEvents = new ImageEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly docDataEvents: DocDataEvents = new DocDataEvents();
  private readonly docUndoRedoEvents: DocUndoRedoEvents = new DocUndoRedoEvents();
  private readonly docEditorEvents: DocEditorEvents = new DocEditorEvents();

  private readonly paragraphHelper: ParagraphHelper = new ParagraphHelper();

  private containerRef!: HTMLElement;

  // Hack: we need to clean DOM first on reload as we mix both intrinsect elements and dom elements (content editable)
  private reloadAfterRender: boolean = false;

  componentWillLoad() {
    this.docEditorEvents.init();
  }

  async componentDidLoad() {
    this.imageEvents.init();
    this.chartEvents.init();

    await this.initOrFetch();
  }

  async disconnectedCallback() {
    this.imageEvents.destroy();
    this.chartEvents.destroy();

    this.docEditorEvents.destroy();

    this.destroy();
  }

  /**
   * Destroy global state and listener.
   */
  private destroy() {
    this.docDataEvents.destroy();
    this.docUndoRedoEvents.destroy();

    editorStore.reset();
    undoRedoStore.reset();
  }

  @Listen('keydown', {target: 'document'})
  onKeyDown($event: KeyboardEvent) {
    const {key, ctrlKey, metaKey} = $event;

    if (key === 'p' && (ctrlKey || metaKey)) {
      this.print($event);
    }
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

    this.initDocEvents(!docId);
    this.initEditable();
    this.initFocus();

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

  private initDocEvents(init: boolean) {
    this.initDocDataEvents(init);
    this.initDocUndoRedoEvents();
  }

  // We init the undo redo oberserver only once rendered as we do not want to add the default title and div to the stack
  private initDocUndoRedoEvents() {
    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      this.docUndoRedoEvents.init(this.containerRef);
    };

    const docObserver: MutationObserver = new MutationObserver(onRender);
    docObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  // If we init, we observe before creating the default elements to persist these but, if we fetch, we observe for changes once everything is loaded
  private initDocDataEvents(init: boolean) {
    if (init) {
      this.docDataEvents.init(this.containerRef);
      return;
    }

    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();

      this.docDataEvents.init(this.containerRef);
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
            </deckgo-doc>

            <AppActionsDocEditor containerRef={this.containerRef}></AppActionsDocEditor>
          </main>
        </ion-content>

        <deckgo-inline-editor
          containers="article"
          sticky-mobile="true"
          img-anchor="deckgo-lazy-img"
          list={true}
          palette={colorStore.state.history}
          align={true}
          fontSize={true}></deckgo-inline-editor>
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
