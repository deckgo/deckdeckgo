import {AuthUser, Doc, formatDate, Paragraph} from '@deckdeckgo/editor';
import {syncStore, errorStore} from '@deckdeckgo/studio';
import {debounce} from '@deckdeckgo/utils';
import {loadingController} from '@ionic/core';
import {Component, ComponentInterface, Fragment, h, JSX, State} from '@stencil/core';
import {AppAnonymousContent} from '../../../components/core/app-anonymous-content/app-anonymous-content';
import {ChartEvents} from '../../../events/core/chart/chart.events';
import {ImageEvents} from '../../../events/core/image/image.events';
import {docs} from '../../../providers/data/doc/doc.provider';
import {getParagraph} from '../../../providers/data/paragraph/paragraph.provider';
import authStore from '../../../stores/auth.store';
import i18n from '../../../stores/i18n.store';
import {Editor} from '../../../types/editor/editor';
import {loadAndImportDoc, navigateReloadEditor} from '../../../utils/core/dashboard.utils';
import {getEdit} from '../../../utils/editor/editor.utils';
import {ParseParagraphsUtils} from '../../../utils/editor/parse-paragraphs.utils';

interface DocAndParagraphs {
  doc: Doc;
  paragraphs: JSX.IntrinsicElements[] | undefined;
}

@Component({
  tag: 'app-docs',
  styleUrl: 'app-docs.scss'
})
export class AppDocs implements ComponentInterface {
  @State()
  private filteredDocs: DocAndParagraphs[] = null;

  @State()
  private loading: boolean = true;

  @State()
  private currentDocId: string | undefined;

  private docs: DocAndParagraphs[] = null;

  private readonly debounceLoading: () => void = debounce(() => (this.loading = false), 750);

  private imageEvents: ImageEvents = new ImageEvents();
  private chartEvents: ChartEvents = new ChartEvents();

  private destroyListener;

  componentWillLoad() {
    this.imageEvents.init();
    this.chartEvents.init();
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initDashboard();
    });

    await this.initDashboard();

    const editor: Editor | undefined = await getEdit();
    this.currentDocId = editor?.type === 'doc' ? editor?.id : undefined;
  }

  disconnectedCallback() {
    this.destroyListener?.();

    this.imageEvents.destroy();
    this.chartEvents.destroy();
  }

  private async initDashboard() {
    if (!authStore.state.loggedIn) {
      this.debounceLoading();
      return;
    }

    this.destroyListener();

    try {
      const userDocs: Doc[] = await docs(authStore.state.authUser.uid);

      this.docs = await this.fetchFirstParagraphs(userDocs);
      this.filterDocs(null);
    } catch (err) {
      errorStore.default.state.error = 'Cannot init your dashboard.';
    }

    this.debounceLoading();
  }

  private async fetchFirstParagraphs(docs: Doc[]): Promise<DocAndParagraphs[]> {
    if (!docs || docs.length <= 0) {
      return [];
    }

    const promises: Promise<DocAndParagraphs>[] = docs
      .filter(({data}: Doc) => data?.paragraphs?.length > 0)
      .map((doc: Doc) => this.initDocAndParagraphs(doc));

    return Promise.all(promises);
  }

  private async initDocAndParagraphs(doc: Doc): Promise<DocAndParagraphs> {
    const paragraphs: Paragraph[] | undefined = await Promise.all(
      doc.data.paragraphs?.map((paragraphId: string) => getParagraph({docId: doc.id, paragraphId}))
    );

    if (!paragraphs) {
      return {
        doc,
        paragraphs: undefined
      };
    }

    const parsedParagraphs: JSX.IntrinsicElements[] = await Promise.all(
      paragraphs
        .filter((paragraph: Paragraph | undefined) => paragraph !== undefined)
        .map((paragraph: Paragraph) => ParseParagraphsUtils.parseParagraph({paragraph}))
    );

    return {
      doc,
      paragraphs: parsedParagraphs
    };
  }

  private filterDocs(value: string | null) {
    if (!value || value === undefined || value === '') {
      this.filteredDocs = this.docs ? [...this.docs] : [];
      return;
    }

    if (!this.docs || this.docs.length <= 0) {
      this.filteredDocs = this.docs ? [...this.docs] : [];
      return;
    }

    const matchingDocs: DocAndParagraphs[] = this.docs.filter((matchDoc: DocAndParagraphs) => {
      return matchDoc.doc?.data?.name?.toLowerCase().indexOf(value.toLowerCase()) > -1;
    });

    this.filteredDocs = [...matchingDocs];
  }

  private filterDocsOnChange($event: CustomEvent) {
    if ($event && $event.detail) {
      this.filterDocs($event.detail.value);
      return;
    }

    this.filterDocs(null);
  }

  private async navigateDoc(doc: DocAndParagraphs) {
    if (!doc || !doc.doc || !doc.doc.id || doc.doc.id === undefined || doc.doc.id === '') {
      return;
    }

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await loadAndImportDoc(doc.doc);

      navigateReloadEditor();
    } catch (err) {
      errorStore.default.state.error = err;
    }

    await loading.dismiss();
  }

  private removeDeletedDoc($event: CustomEvent) {
    if (!$event || !$event.detail || $event.detail === undefined || $event.detail === '') {
      return;
    }

    const docId: string = $event.detail;

    const index: number = this.findDocIndex(docId);

    if (index < 0) {
      return;
    }

    this.docs.splice(index, 1);

    this.filterDocs(null);
  }

  private findDocIndex(id: string): number {
    if (!this.docs || this.docs.length < 0) {
      return -1;
    }

    if (!id || id === undefined || id === '') {
      return -1;
    }

    return this.docs.findIndex((matchDoc: DocAndParagraphs) => {
      return matchDoc?.doc?.id === id;
    });
  }

  render() {
    return (
      <Fragment>
        <app-navigation></app-navigation>
        <ion-content class="ion-padding">{this.renderContent()}</ion-content>
      </Fragment>
    );
  }

  private renderContent() {
    if (this.loading) {
      return <app-spinner></app-spinner>;
    }

    if (!authStore.state.authUser) {
      return <AppAnonymousContent title={i18n.state.menu.documents} text={i18n.state.settings.access_docs}></AppAnonymousContent>;
    }

    return (
      <main class="ion-padding fit">
        <h1>{i18n.state.dashboard.your_documents}</h1>

        {this.renderDocsFilter()}

        {this.renderDocs()}
      </main>
    );
  }

  private renderDocsFilter() {
    if (this.filteredDocs?.length > 0) {
      return (
        <ion-searchbar
          debounce={500}
          animated={false}
          placeholder={i18n.state.dashboard.filter}
          onClick={($event) => $event.stopImmediatePropagation()}
          onIonChange={($event: CustomEvent) => this.filterDocsOnChange($event)}
          class="ion-no-padding ion-margin-top ion-margin-bottom"
        />
      );
    }

    if (syncStore.default.state.dirty) {
      return <p>{i18n.state.dashboard.sync_docs}</p>;
    }

    return <p>{i18n.state.dashboard.no_docs}</p>;
  }

  private renderDocs() {
    if (this.filteredDocs?.length > 0) {
      return <div class="container">{this.renderDocsCards()}</div>;
    }

    return undefined;
  }

  private renderDocsCards() {
    return this.filteredDocs.map((doc: DocAndParagraphs) => {
      if (doc === undefined) {
        return undefined;
      }

      return (
        <article key={doc.doc.id}>
          <ion-card custom-tappable class="item ion-no-margin" onClick={() => this.navigateDoc(doc)}>
            {this.renderDoc(doc)}
          </ion-card>

          {this.renderAside(doc)}
        </article>
      );
    });
  }

  private renderAside(doc: DocAndParagraphs) {
    return (
      <aside>
        <p>{doc.doc.data.name}</p>
        <p>{formatDate(doc.doc.data.updated_at)}</p>

        <app-dashboard-actions
          data={{doc: doc.doc}}
          disableDelete={doc.doc.id === this.currentDocId}
          onDeleted={($event: CustomEvent) => this.removeDeletedDoc($event)}
          onCloned={() => navigateReloadEditor()}></app-dashboard-actions>
      </aside>
    );
  }

  private renderDoc(doc: DocAndParagraphs) {
    return <deckgo-doc>{doc.paragraphs}</deckgo-doc>;
  }
}
