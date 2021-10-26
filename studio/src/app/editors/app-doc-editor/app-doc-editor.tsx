import {Component, ComponentInterface, Fragment, h, JSX, Method, State} from '@stencil/core';

import {v4 as uuid} from 'uuid';

import colorStore from '../../stores/color.store';
import i18n from '../../stores/i18n.store';

import {Editor} from '../../types/editor/editor';

import {getEdit} from '../../utils/editor/editor.utils';
import {ImageEvents} from '../../events/core/image/image.events';
import {ChartEvents} from '../../events/core/chart/chart.events';
import {DocEvents} from '../../events/editor/doc/doc.events';
import {SectionHelper} from '../../helpers/editor/section.helper';

@Component({
  tag: 'app-doc-editor',
  styleUrl: 'app-doc-editor.scss'
})
export class AppDocEditor implements ComponentInterface {
  @State()
  private docFetched: boolean = false;

  @State()
  private sections: JSX.IntrinsicElements[] = [];

  private readonly imageEvents: ImageEvents = new ImageEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly docEvents: DocEvents = new DocEvents();

  private readonly sectionHelper: SectionHelper = new SectionHelper();

  private docRef!: HTMLDeckgoDocElement;

  async componentDidLoad() {
    await this.imageEvents.init();
    await this.chartEvents.init();

    this.docEvents.init(this.docRef);

    await this.initOrFetch();
  }

  async disconnectedCallback() {
    this.imageEvents.destroy();
    this.chartEvents.destroy();

    this.docEvents.destroy();
  }

  @Method()
  async initNewDoc() {
    this.docFetched = false;

    await this.initOrFetch();
  }

  private async initOrFetch() {
    const editor: Editor | undefined = await getEdit();
    const docId: string | undefined = editor?.id;

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

    const Section = 'section';

    const section: JSX.IntrinsicElements = <Section key={uuid()}>Yolo</Section>;

    // We observe the first mutation to persist the created data
    this.docEvents.observeCreateDoc();

    this.sections = [title, section];
  }

  private async fetchDoc(docId: string) {
    const sections: JSX.IntrinsicElements[] = await this.sectionHelper.loadDocAndRetrieveSections(docId);
    this.sections = sections?.length > 0 ? [...sections] : [];
  }

  render() {
    return (
      <Fragment>
        <ion-content class={`ion-no-padding`}>
          <main>
            {this.renderLoading()}
            <deckgo-doc ref={(el) => (this.docRef = el as HTMLDeckgoDocElement)}>
              <article contentEditable={true}>{this.sections}</article>
            </deckgo-doc>
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
