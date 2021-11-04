import {Component, ComponentInterface, Fragment, h, JSX, Method, State} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import colorStore from '../../stores/color.store';
import i18n from '../../stores/i18n.store';

import {Editor} from '../../types/editor/editor';
import {SlotType} from '../../types/editor/slot-type';

import {ImageEvents} from '../../events/core/image/image.events';
import {ChartEvents} from '../../events/core/chart/chart.events';
import {DocEvents} from '../../events/editor/doc/doc.events';
import {DocEditorEvents} from '../../events/editor/editor/doc-editor.events';

import {ParagraphHelper} from '../../helpers/editor/paragraphHelper';

import {getEdit} from '../../utils/editor/editor.utils';

import {AppActionsDocEditor} from '../../components/editor/doc/app-actions-doc-editor/app-actions-doc-editor';
import editorStore from '../../stores/editor.store';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  @State()
  private docFetched: boolean = false;

  @State()
  private paragraphs: JSX.IntrinsicElements[] = [];

  private readonly imageEvents: ImageEvents = new ImageEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly docEvents: DocEvents = new DocEvents();
  private readonly docEditorEvents: DocEditorEvents = new DocEditorEvents();

  private readonly paragraphHelper: ParagraphHelper = new ParagraphHelper();

  private containerRef!: HTMLElement;

  // Hack: we need to clean DOM first on reload as we mix both intrinsect elements and dom elements (content editable)
  private reloadAfterRender: boolean = false;

  componentWillLoad() {
    this.docEditorEvents.init();
  }

  async componentDidLoad() {
    await this.imageEvents.init();
    await this.chartEvents.init();

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
   * @private
   */
  private destroy() {
    this.docEvents.destroy();
    editorStore.reset();
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
    this.docFetched = false;

    await this.initOrFetch();
  }

  private async initOrFetch() {
    const editor: Editor | undefined = await getEdit();
    const docId: string | undefined = editor?.id;

    this.initDocEvents(!docId);
    this.initEditable();

    if (!docId) {
      await this.initDoc();
    } else {
      await this.fetchDoc(docId);
    }

    this.docFetched = true;
  }

  private async initDoc() {
    const Title = 'h1';
    const title: JSX.IntrinsicElements = <Title key={uuid()}>Title</Title>;

    this.paragraphs = [title, this.emtpySection()];
  }

  private emtpySection(): JSX.IntrinsicElements {
    const Section = 'section';
    return (
      <Section key={uuid()}>
        <br />
      </Section>
    );
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

  // If we init, we observe before creating the default elements to persist these but, if we fetch, we observe for changes once everything is loaded
  private initDocEvents(init: boolean) {
    if (init) {
      this.docEvents.init(this.containerRef);
      return;
    }

    const onRender = (_mutations: MutationRecord[], observer: MutationObserver) => {
      observer.disconnect();
      this.docEvents.init(this.containerRef);
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
    if (this.docFetched) {
      return undefined;
    } else {
      return (
        <app-spinner>
          <p>{i18n.state.editor.loading}</p>
        </app-spinner>
      );
    }
  }
}
